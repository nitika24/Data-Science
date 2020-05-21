Sys.setlocale('LC_ALL','C')

sms = read.csv(file.choose())
str(sms)
round(prop.table(table(sms$type))*100, digits = 1)
sms$type = factor(sms$type)
#install.packages("tm")
library(tm)
sms_corpus = Corpus(VectorSource(sms$text))
print(sms_corpus)
inspect(sms_corpus[1:3])

sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)                    # convert to lower case
corpus_clean <- tm_map(corpus_clean, removeNumbers)            # remove digits
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) # and but or you etc
corpus_clean <- tm_map(corpus_clean, removePunctuation)        # you'll never guess!
corpus_clean <- tm_map(corpus_clean, stripWhitespace)          # reduces w/s to 1
inspect(corpus_clean[1:3])



# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm
str(sms_dtm)
View(sms_dtm)
print(sms_dtm)

# creating training and test datasets
sms_train <- sms[1:4169, ]
sms_test  <- sms[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_train$type))
prop.table(table(sms_test$type))

install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_train,
          min.freq=40,          # 10% of num docs in corpus is rough standard
          random.order = FALSE) # biggest words are nearer the centre

spam <- subset(sms_train, type == "spam")
ham  <- subset(sms_train, type == "ham")

wordcloud(spam$text,
          max.words=40,     # look at the 40 most common words
          scale=c(3, 0, 5)) # adjust max and min font sizes for words shown

wordcloud(ham$text,
          max.words=40,     # look at the 40 most common words
          scale=c(3, 0, 5)) # adjust max and min font sizes for words shown

# indicator features for frequent words
# dictionary of words which are used more than 5 times
sms_dict <- findFreqTerms(sms_dtm_train, 5)

reduced_sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
reduced_sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# have we reduced the number of features?
ncol(reduced_sms_train)

ncol(reduced_sms_test)

# convert counts to a factor
# custom function: if a word is used more than 0 times then mention 1 else mention 0
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
# Margin = 2 is for columns
# Margin = 1 is for rows
reduced_sms_train <- apply(reduced_sms_train, MARGIN = 2, convert_counts)
reduced_sms_test  <- apply(reduced_sms_test, MARGIN = 2, convert_counts)

##  Training a model on the data ----
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(reduced_sms_train, sms_train$type)
sms_classifier

##  Evaluating model performance ----
class(sms_classifier)
class(sms_test)
sms_test_pred <- predict(sms_classifier, reduced_sms_test)

table(sms_test_pred)
prop.table(table(sms_test_pred))

library(gmodels)
CrossTable(sms_test_pred, sms_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))



 #Once again, we get an impressive out-of-the-box result: 97.5% correct!  however:

#32 spam messages avoided detection (17.5%)
#4 ham messages got trashed (0.3%)
###########################################
