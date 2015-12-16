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

#explore n-grams in each corpus:
#Total terms

#unique unigrams
sapply(dfm.en, ncol)
#twitter    news   blogs 
#419417  331755  380867 
#unique bigrams
sapply(bigram, function(x) length(unique(unlist(x))))
#twitter    news   blogs 
#5281764 6296250 6341521 
#unique trigrams
sapply(trigram, function(x) length(unique(unlist(x))))
# twitter     news    blogs 
#13581787 17875031 18951463 

#consider leaving in numbers so we dont have incomplete ngrams. or filter out at the gram stage
#handle twitter mentions. otherwise replace @ with "at"
#twitter hashtag removal. just remove octothorpe if it at the end of a tweet, otherwise,  gaps
#break sentences on punctuation
#recreate grams with space instead of underscore - easier to match on later

#calculate frequencies
createModel <- function(gram, n){
  #assumes list of corpora
  
  gram <- lapply(gram, unlist)
  #rounding to 8 works for sample, may need to increase precision for larger corpora
  #ie length of samp[[1]] == 114293. 1/114293 = 8.7e-6. can round after that (keep 2 extra JICOC)
  
  freq <- lapply(gram, function(x) round(table(x)/length(x), 8))
  tf <- lapply(freq, function(x) as.data.frame(x, row.names = NULL, stringsAsFactors = F))
  rm(freq)
  #here we see the top relative frequencies
  #lapply(tf, function(x) head(x[order(x$Freq, decreasing = T),], 50))
  #while the most popular tokens were different, the most popular grams are similar across each of the
  #sources - the grams that help build sentences, "of the", "in the", "to the"
  
  #remove grams with profanity
  sw <- paste0("(([^a-z]+|^)(", 
               paste(readLines(paste0("https://github.com/shutterstock/",
                               "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/raw/master/en")
                               ), sep = "", collapse = "|")
               , ")([^a-z]+|$)|", paste0(readLines("stop.txt"), sep = "", collapse = "|"), ")")
  tf <- lapply(tf, function(x) x[-grep(sw, x$x), ])
  rm(sw)
  
  #Indexing. For bigrams, only take [1] the first word to index
  #for n>2, need to combine 1 - n-1 into a single word index
  for (i in 1:length(tf)){
    t <- unlist(lapply(strsplit(tf[[i]]$x, "_"), function(x) x[1:n-1]))
    p <- t[seq(1, length(t), by = n - 1)]
    #process additional grams if n > 2
    for(j in 2:(n - 1)) p <- paste(p, t[seq(j, length(t), by = n - 1)])
    tf[[i]]$idx <- p
  }
  
  #only keep the most frequent ngram by index
  #we can go back and find term(s) matching this max freq and idx
  lookup <- lapply(tf, function(x) setNames(aggregate(x$Freq, by = list(x$idx), max), 
                                            c("idx", "Freq")))
  
  model <- list()
  for (i in 1:length(tf)) model[[i]] <- merge(lookup[[i]], tf[[i]])
  #remove duplicate idx and freq, taking the first (alpha) occurance. ok to drop freq at this point
  model <- lapply(model, function(x) x[!duplicated(x$idx), c("idx", "x")])
  #sanity check, "i love" highest freq for index "i" in twitter
  #tf[[1]][tf[[1]]$idx == "i", ][order(tf[[1]][tf[[1]]$idx == "i", "Freq"]), ]
  #model.bi[[1]][model.bi[[1]]$idx == "i",]
  rm(gram); rm(tf); rm(lookup)
  #need to combine back into 1. need to figure out how to deal with mismatching index/freq combos
  model
}

#Index n-grams
bigram <- readRDS("data/bigram_EN.RDS")
bi.samp <- lapply(bigram, function(x) x[1:10000])
rm(bigram)
trigram <- readRDS("data/trigram_EN.RDS")
tri.samp <- lapply(trigram, function(x) x[1:10000])
rm(trigram)
model.bi <- createModel(bi.samp)
model.tri <- createModel(tri.samp)

