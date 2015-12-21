library(tm)
library(Matrix)
library(plyr)

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

#break up sentences by period, only for news. blogs and twitter have less reliable breaks
en[["news"]] <- unlist(stri_split_boundaries(en[["news"]], type = "sentence", 
                                             skip_sentence_sep = T))

corp <- lapply(en, corpus)
rm(en)


# a simple vector version in case we can use in the future. much smaller.
#tok <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
#                                                 removeSeparators = T, simplify = T )))
tok.list <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
                                                      removeSeparators = T)))

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
quadgram <- lapply(corp, function(x) tokenize(toLower(x), removeNumbers = T, removePunct = T, 
                                              removeSeparators = T, ngrams = 3))
quadgram <- list()
system.time(twitter <- tokenize(toLower(corp[[1]]), removeNumbers = F, removePunct = T, 
                                removeSeparators = T, ngrams = 4))
#need to remove remaining puct (apostrophes)
head(bigram.test[[1]])


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
sapply(quadgram.samp, function(x) length(unique(unlist(x))))
#blogs    news twitter 
#372512  143274   94941 

#handle twitter mentions. otherwise replace @ with "at"
#twitter hashtag removal. just remove octothorpe if it at the end of a tweet, otherwise,  gaps
#break sentences on punctuation
#recreate grams with space instead of underscore - easier to match on later

#calculate frequencies
createModel <- function(gram, n){
  #assumes list of corpora
  print("create model begin")
  print(timestamp())
  gram <- lapply(gram, unlist)
  #rounding to 8 works for sample, may need to increase precision for larger corpora
  #ie length of samp[[1]] == 114293. 1/114293 = 8.7e-6. can round after that (keep 2 extra JICOC)

  print("creating freq tables")
  print(timestamp())
  
  f<- lapply(gram, function(x) table(x))
  rf <- lapply(gram, function(x) round(table(x)/length(x), 8))
  tf <- list()
  for(i in 1:length(rf)){
    tf[[i]] <- as.data.frame(rf[[i]], row.names = NULL, stringsAsFactors = F)
    tf[[i]]$abs <- f[[i]]
  }
  rm(f); rm(rf);
  
  print("gram filtering")
  print(timestamp())
  
  #lapply(tf, function(x) head(x[order(x$Freq, decreasing = T),], 50))
  #here we see the top relative frequencies
  #while the most popular tokens were different, the most popular grams are similar across each of
  #the sources - the grams that help build sentences, "of the", "in the", "to the"
  
  #GRAM FILTERING
  
  #remove grams with words that appear in the profrane list
  sw <- paste0("(([^a-z]+|^)(", 
               paste(readLines(paste0("https://github.com/shutterstock/",
                                      "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/raw/master/en")
               ), sep = "", collapse = "|")
               , ")([^a-z]+|$))")
  #remove grams with profanity within the word
  sw <- paste0(sw, "|", paste0(readLines("stop.txt"), sep = "", collapse = "|"))
  #remove grams with numbers
  sw <- paste0(sw, "|[0-9]")
  #remove grams with #hashtags and @mentions
  sw <- paste0(sw, "|[#@]")
  
  tf.s <- list()
  for (i in 1:length(tf)){
    #lapply did not work here
    idx <- grep(sw, tf[[i]]$x)
    if (length(idx) == 0) tf.s[[i]] <- tf[[i]]
    if (length(idx) > 0) tf.s[[i]] <- tf[[i]][-idx, ]
  }
  rm(tf);  rm(sw);

  print("indexing")
  print(timestamp())
  
  #Indexing. For bigrams, only take [1] the first word to index
  #for n>2, need to combine 1 - n-1 into a single word index
  for (i in 1:length(tf.s)){
    t <- unlist(lapply(strsplit(tf.s[[i]]$x, "_"), function(x) x[1:n-1]))
    p <- t[seq(1, length(t), by = n - 1)]
    #process additional grams if n > 2
    if (n > 2)  for(j in 2:(n - 1))  p <- paste(p, t[seq(j, length(t), by = n - 1)])
    tf.s[[i]]$idx <- p
  }
  
  print("lookup")
  print(timestamp())
  
  #only keep the most frequent ngram by index
  lookup <- lapply(tf.s, function(x) setNames(aggregate(x$Freq, by = list(x$idx), max), 
                                              c("idx", "Freq")))
 # if we take only trigram indexes that appear more than once, we have 20943 out of 319675 ~ 6%
  
  print("create model")
  print(timestamp())
  
  model <- list()
  for (i in 1:length(tf.s)){
    model[[i]] <- merge(lookup[[i]], tf.s[[i]])
    #remove duplicate idx and freq, taking the first (alpha) occurance
    #only take freq or rel freq in next step, depending on final implementation
    model[[i]] <- model[[i]][!duplicated(model[[i]]$idx), ]
    #remove first n-1 grams and keep only the term to predict
    model[[i]]$x <- unlist(lapply(strsplit(model[[i]]$x, "_"), function(x) unclass(x)[n]))
  }
  #sanity check, "i love" highest freq for index "i" in twitter
  #tf[[1]][tf[[1]]$idx == "i", ][order(tf[[1]][tf[[1]]$idx == "i", "Freq"]), ]
  #model.bi[[1]][model.bi[[1]]$idx == "i",]
  rm(gram); rm(tf.s); rm(lookup)
  
  #need to combine back into 1 and figure out how to deal with mismatching index combos. I think #1
  #1. hang on to frequency count still. whoever has more observations wins
  #use same process as when there multiple indexes before. combine into a single and lookup the max
  #with this method we can combine earlier on in the model building
  #2. whoever has higher RELATIVE freq wins
  
  model <- rbind.fill(model)
  
  print("create singular lookup")
  print(timestamp())
  
  lookup <- setNames(aggregate(model$abs, by = list(model$idx), max), c("idx", "maxAbs"))
  
  print("final merge")
  print(timestamp())
  
  final <- merge(lookup, model, by.x = c("idx", "maxAbs"), by.y = c("idx", "abs"))
 
  final[, c("idx", "x")]
}

#Index n-grams/create model
bigram <- readRDS("data/bigram_EN.RDS")
bi.samp <- lapply(bigram, function(x) x[1:10000])
rm(bigram)
trigram <- readRDS("data/trigram_EN.RDS")
tri.samp <- lapply(trigram, function(x) x[1:10000])
rm(trigram)
model.bi <- createModel(bi.samp, 2)
model.tri <- createModel(tri.samp, 3)
quadgram.samp <- lapply(quadgram, function(x) x[1:10000])
model.quad <- createModel(quadgram.samp, 4)

system.time(model.tri <- createModel(readRDS("data/trigram_EN.RDS"), 2))
saveRDS(model.tri, "model/model_tri_EN.RDS")
system.time(model.quad <- createModel(readRDS("data/quadgram_EN.RDS"), 4))
saveRDS(model.quad, "model/model_quad_EN.RDS")



#backoff and smoothing

#consider using unigrams from news dataset as a dictionary. combine that with a frequency threshold
#so things like lol and omg (both high frequency) don't get filtered out. 