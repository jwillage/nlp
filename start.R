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

#break up sentences by period, only for news. TODO blogs and twitter have less reliable breaks
en[["news"]] <- unlist(stri_split_boundaries(en[["news"]], type = "sentence", 
                                             skip_sentence_sep = T))
saveRDS(en, "data/readLines_list_all_EN.RDS")

corp <- lapply(en, corpus)
rm(en)


trigram[["twitter"]] <- tokenize(toLower(corp[[1]]), removePunct = T, removeSeparators = T, 
                                 concatenator = " ", ngrams = 3)
trigram[["news"]] <- tokenize(toLower(corp[[2]]), removePunct = T, removeSeparators = T, 
                              concatenator = " ", ngrams = 3)
trigram[["blogs"]] <- tokenize(toLower(corp[[3]]), removePunct = T, removeSeparators = T, 
                              concatenator = " ", ngrams = 3)
saveRDS(trigram, "data/trigram_EN_new.RDS")

#build n-grams
unigram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = T, removeSeparators = T, 
                                             concatenator = " ", ngrams = 1))
unigram.samp.100000 <- lapply(unigram, sample, 100000)
bigram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = T, removeSeparators = T, 
                                            concatenator = " ", ngrams = 2))
bigram.samp.100000 <- lapply(bigram, sample, 100000)
trigram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = T, removeSeparators = T, 
                                            concatenator = " ", ngrams = 3))
trigram.samp.100000 <- lapply(trigram, sample, 100000)

quadgram <- lapply(corp, function(x) tokenize(toLower(x), removeNumbers = T, removePunct = T, 
                                              removeSeparators = T, ngrams = 3))
quadgram <- list()
system.time(twitter <- tokenize(toLower(corp[[1]]), removeNumbers = F, removePunct = T, 
                                removeSeparators = T, ngrams = 4))


#handle twitter mentions. otherwise replace @ with "at"
#twitter hashtag removal. just remove octothorpe if it at the end of a tweet, otherwise,  gaps

createModel <- function(gram, n){
  #assumes list of corpora
  print(paste(timestamp(quiet = T), "create model begin"))
  gram <- lapply(gram, unlist)
  #rounding to 8 works for sample, may need to increase precision for larger corpora
  #ie length of samp[[1]] == 114293. 1/114293 = 8.7e-6. can round after that (keep 2 extra JICOC)

  #filter out apostrophes
  print(paste(timestamp(quiet = T), "filtering apostrophes"))
  gram <- lapply(gram, function(x) gsub("'", "", unlist(x), fixed = T, perl = T))
  #make sure rownames are not taking up space, else set to NULL
  
  print(paste(timestamp(quiet = T), "creating freq table"))
  f<- lapply(gram, function(x) table(x))
  rf <- lapply(gram, function(x) round(table(x)/length(x), 8))
  tf <- list()
  for(i in 1:length(rf)){
    tf[[i]] <- as.data.frame(rf[[i]], row.names = NULL, stringsAsFactors = F)
    tf[[i]]$abs <- f[[i]]
  }
  rm(f); rm(rf);
  
  print(paste(timestamp(quiet = T), "gram filtering"))
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
  #remove grams with remaining special chars
  sw <- paste0(sw, "|_")
  
  tf.s <- list()
  for (i in 1:length(tf)){
    #lapply did not work here
    idx <- grep(sw, tf[[i]]$x, perl = TRUE)
    if (length(idx) == 0) tf.s[[i]] <- tf[[i]]
    if (length(idx) > 0) tf.s[[i]] <- tf[[i]][-idx, ]
  }
  rm(tf);  rm(sw);

  #Indexing. For bigrams, only take [1] the first word to index
  #for n>2, need to combine 1 - n-1 into a single word index
  for (i in 1:length(tf.s)){
    print(paste(timestamp(quiet = T), "index unlisting"))
    t <- unlist(lapply(strsplit(tf.s[[i]]$x, " "), function(x) x[1:n-1]))
    print(paste(timestamp(quiet = T), "index subsetting"))
    #needs to be on unlisted and 1 big blob of text
    p <- t[seq(1, length(t), by = n - 1)]
    #process additional grams if n > 2
    print(paste(timestamp(quiet = T), "index additional grams"))
    if (n > 2)  for(j in 2:(n - 1))  p <- paste(p, t[seq(j, length(t), by = n - 1)])
    tf.s[[i]]$idx <- p
  }
  
  print(paste(timestamp(quiet = T), "lookup"))
  #only keep the most frequent ngram by index
  lookup <- lapply(tf.s, function(x) setNames(aggregate(x$Freq, by = list(x$idx), max), 
                                              c("idx", "Freq")))
  #here look at filtering out grams with freq == 1
  
  print(paste(timestamp(quiet = T), "create model"))
  model <- list()
  for (i in 1:length(tf.s)){
    print(paste(timestamp(quiet = T), "model merge"))
    model[[i]] <- merge(lookup[[i]], tf.s[[i]])
    #remove duplicate idx and freq, taking the first (alpha) occurance
    #only take freq or rel freq in next step, depending on final implementation
    print(paste(timestamp(quiet = T), "model remove dups"))
    model[[i]] <- model[[i]][!duplicated(model[[i]]$idx), ]
    #remove first n-1 grams and keep only the term to predict
    print(paste(timestamp(quiet = T), "model unlisting"))
    model[[i]]$x <- unlist(lapply(strsplit(model[[i]]$x, " "), function(x) unclass(x)[n]))
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
  
  print(paste(timestamp(quiet = T), "rbind list"))
  model <- rbind.fill(model)
  
  print(paste(timestamp(quiet = T), "aggregate"))
  lookup <- setNames(aggregate(model$abs, by = list(model$idx), max), c("idx", "maxAbs"))
  
  print(paste(timestamp(quiet = T), "merge final"))
  final <- merge(lookup, model, by.x = c("idx", "maxAbs"), by.y = c("idx", "abs"))
 
  #todo remove dups, short-term only return the first from model if +1 are returned.
  #add freq in the return
  final[, c("idx", "x")]
}

#TODO need unigram freq (to use for parts of speech)

#Index n-grams/create model
model.tri <- createModel(tri.samp, 3)



#backoff and smoothing

stupidBackoff <- function(phrase){
  tri <- trigram.model.samp[trigram.model.samp$idx == phrase, "x"]
  bi <- bigram.model.samp.100000[bigram.model.samp.100000$idx == phrase, "x"]
  ifelse (length(tri) > 0, tri, 
          ifelse (length(bi) > 0, bi,
                  #Implement POS tagger
                  "the"
                  )
          )
}

#consider using unigrams from news dataset as a dictionary. combine that with a frequency threshold
#so things like lol and omg (both high frequency) don't get filtered out. 

#create test set and see model accuracy