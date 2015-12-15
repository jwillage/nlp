library(tm)
library(Matrix)

fn <- "data/Coursera-SwiftKey.zip"
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                     , fn)
l <- unzip(fn, list = T)

# #work with text documents in list format
tmp <- list()
#for (i in l$Name[grep("US.*txt", l$Name)])
for (i in l$Name)
  tmp[[length(tmp) + 1]] <- readLines(unz("data/Coursera-SwiftKey.zip", i))
tmp <- setNames(tmp, gsub(".txt", "" ,gsub(".*/.*/", "", l$Name[grep("txt", l$Name)])))
saveRDS(tmp, "data/readLines_list_allFiles.RDS")

en <- list()
en[["twitter"]] <- tmp[[7]]; en[["news"]] <- tmp[[8]]; en[["blogs"]] <- tmp[[9]];
rm(tmp)
saveRDS(en, "data/readLines_list_all_EN.RDS")

lapply(sapply(en, nchar), max)
#the longest document is in the blogs data set, over 40K characters

corp <- lapply(en, corpus)
rm(en)

#when ready, combine into single corpus
#t <- c(corp.samp[[1]], corp.samp[[2]], corp.samp[[3]])

#tokenize and remove stopwords, etc.
#we make the decision to remove punctuation, numbers, whitepsace, stopwords, change to lower

# a simple vector version in case we can use in the future. much smaller.
#tok <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
#                                                 removeSeparators = T, simplify = T )))
tok.list <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
                                                 removeSeparators = T)))
#remove profane stopwords
tok.list.s <- lapply(tok.list, function(x) removeFeatures(x, readLines("stop.txt")))
tok.list <- tok.list.s
rm(tok.list.s)

#create document frequency matrices
dfm.en <- lapply(tok.list, dfm)

#think about removing terms that only appear once?
#try removeSparseTerms(bigTDM, sparse= 0.8)

#view 20 most frequent tokens
lapply(dfm.en, topfeatures)

#we see the top 50 tokens for twitter, news, blogs
#the topfeatures show how conversational (and narcissistic) twitter is. 
#news seems more objective, with "said", "he", "was", etc appearing near the top of the list
#blogs looks like it's in-between twitter and news, as we might imagine.

sapply(dfm.en, ncol)
#twitter has the most diverse set of terms, over 400K

#build n-grams
bigram.test <- lapply(corp, function(x) tokenize(x, removeNumbers = T, removePunct = T, 
                                                    removeSeparators = T, ngrams = 2, simplify = T))
bigram <- lapply(corp, function(x) tokenize(toLower(x), removeNumbers = T, removePunct = T, 
                                                    removeSeparators = T, ngrams = 2))
trigram <- lapply(corp, function(x) tokenize(toLower(x), removeNumbers = T, removePunct = T, 
                                            removeSeparators = T, ngrams = 3))

#explore n-grams in each corpus

#Total terms

#unique unigrams
sapply(dfm.en, ncol)
#twitter    news   blogs 
#419417  331755  380867 
#unique bigrams

#unique trigrams
sapply(trigram, function(x) length(unique(unlist(x))))
# twitter     news    blogs 
#13581787 17875031 18951463 

#leave in profranity and manually give them a 0 probability. This way we don't make weird n-grams
#of incomplete sentences. 
