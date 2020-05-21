setwd("D:\\Nitika\\Data Science\\Assignments\\Text Mining")

install.packages("tidyverse")
install.packages("readr")
install.packages("kableExtra")
install.packages("DT")
install.packages("lubridate")
install.packages("tidytext")
install.packages("rio")


library(tidyverse)  #to visualize, transform, input, tidy and join data
library(readr)      #to input data from tsv
library(dplyr)      #data wrangling
library(kableExtra) #to create HTML Table
library(DT)         #to preview the data sets
library(lubridate)  #to apply the date functions

library(tm)         #to text mine
library(wordcloud)  #to build word cloud
library(tidytext)   #to text mine
library(rvest)
library(XML)
library(magrittr)


# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Echo-Dot-3rd-Gen-improved/product-reviews/B07PFFMP9P/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  #rate <- murl %>%
   # html_nodes(".review-rating") %>%
    #html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"alexa.txt",row.names = FALSE)

alexa <- read.delim('alexa.txt')

#Below the location of the positive and negative words
pos = scan('D:\\Nitika\\Data Science\\Assignments\\Text Mining\\positive-words.txt', what='character', comment.char=';')
neg = scan('D:\\Nitika\\Data Science\\Assignments\\Text Mining\\negative-words.txt', what='character', comment.char=';')

str(alexa)


View(alexa)

# Build Corpus and DTM/TDM
library(tm)
corpus <- alexa[-1,]
head(corpus)

class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))

cleanset<-tm_map(cleanset,removeWords, c('can','film'))

# Since the word laptop and can were used, this can be removed as we are 
# mining the tweets for this film.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
# Removing the word movie and movies on similar grounds - as unnecessary.


cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')

# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 13649 words and 393036 documents(# of tweets) in this TDM
# Sparsity is 97% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word alexa,Like and James as the highest frequency. This implies
# that Movie alexa has got more reviews about the James and 
# most of them liked the movie.

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# lettercloud 

letterCloud(w,word = 'A',frequency(5), size=1)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# install.packages("syuzhet")

# Read File 
amazon_reviews <- read.delim('alexa.TXT')
reviews <- as.character(amazon_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)

head(s)

reviews[4]

# Watched it!!! And have no words to describe the splendid performance of the cast and how beautifully wan visualised and presented this..This is the true jewel so far in DC universe<U+0001F44D><U+0001F3FB>\nWon't Add any Spoilers<U+0001F60B>..as this is something you should witness"
# on tweet 4, you have 4 for anger, each one work for disgust, fear 
# and sadness, 3 for trust , 4 words for negative and 2 positive.
get_nrc_sentiment('splendid')

# Splendid has one Joy and one positive 
get_nrc_sentiment('no words') #1 Anger and 1 Negative

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for alexa')


#Inferences
#alexa movie has more positive reviews compared to negative