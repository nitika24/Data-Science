install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("httpuv")
install.packages("tm")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("reshape2")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("wordcloud")

library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(stringr)
library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)


library(reshape2)
library(dplyr)
library(arules)
library(arulesViz)
library(corpcor)
library(SnowballC)

Sys.setlocale('LC_ALL','C')

cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',                accessURL='https://api.twitter.com/oauth/access_token',                 authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret

#Extracting tweets

facebook<-searchTwitter("facebook", n=1500)

#Below the location of the positive and negative words
pos = scan('D:\\Nitika\\Data Science\\Assignments\\Text Mining\\positive-words.txt', what='character', comment.char=';')
neg = scan('D:\\Nitika\\Data Science\\Assignments\\Text Mining\\negative-words.txt', what='character', comment.char=';')

#Cleaning data

??twListToDF
??Corpus
??gsub
??tm_map
df1 <- twListToDF(facebook)
myCorpus <- Corpus(VectorSource(df1$text))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- Corpus(VectorSource(myCorpus))

#Creating term document matrix

wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}


tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

#Finding most frequent words

(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

#Removing stop words

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"and", "when", "what", "to", "this","the","that","so","of","it","is","in","at","a","be","by","for","have","on","our","are","i","will","with","you")

#Creating Wordcloud

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
library(wordcloud)
wordcloud(myCorpus ,max.words =150,min.freq=3,scale=c(4,.5),colors=palette())

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

#Most frequent words after removing stop words

(freq.terms <- findFreqTerms(tdm, lowfreq = 50))

#Plot of most frequent words

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 50)
df2 <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df2, aes(x=term, y=freq)) + geom_bar(stat="identity") +xlab("Terms") + ylab("Count") + coord_flip() +theme(axis.text=element_text(size=7))

#Calculating the sentiment score

df <- twListToDF(facebook)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste("facebook", '_stack.csv'))==FALSE) write.csv(df, file=paste("facebook", '_stack.csv'), row.names=F) 
stack <- read.csv(file=paste("facebook", '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste("facebook", '_stack.csv'), row.names=F)

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

Dataset <- stack
Dataset$text <- as.factor(Dataset$text)
Dataset$text<- str_replace_all(Dataset$text,"ν ½ν²Έν ½ν²°' "," ")
scores <- score.sentiment(Dataset$text, pos, neg, .progress='text')

write.csv(scores, file=paste("facebook", '_scores.csv'), row.names=TRUE)
stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))

by.tweet <- group_by(stat, tweet, created)

by.tweet <- dplyr:: summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste("facebook", '_opin.csv'), row.names=TRUE)

#Plot of the sentiment analysis
ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
  geom_point(aes(group=tweet, color=tweet), size=4) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
ggtitle(facebook)

#Inferences:
#Neutral tweets are more as compared to positive and negative by each day.
#There is reduction in neutral tweets by 80.
#Negative tweets are constant everyday
#Positive tweets has a slight reduction.
