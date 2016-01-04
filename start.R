library(tm)
library(Matrix)
library(plyr)
library(quanteda)
library(jsonlite)

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
quadgram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = T, removeSeparators = T, 
                                concatenator = " ", ngrams = 4))
quadgram.samp.100000 <- lapply(quadgram, sample, 100000)
rm(corp)


#handle twitter mentions. otherwise replace @ with "at"
#twitter hashtag removal. just remove octothorpe if it at the end of a tweet, otherwise,  gaps

createModel <- function(gram, n){
  print(paste(timestamp(quiet = T), "create model begin"))
  gram <- unlist(gram)
  #rounding to 8 works for sample, may need to increase precision for larger corpora
  #ie length of samp[[1]] == 114293. 1/114293 = 8.7e-6. can round after that (keep 2 extra JICOC)

  #filter out apostrophes
  print(paste(timestamp(quiet = T), "filtering apostrophes"))
  gram <- gsub("'", "", unlist(gram), fixed = T, perl = T)
  #make sure rownames are not taking up space, else set to NULL
  
  print(paste(timestamp(quiet = T), "creating freq table"))
  f<- table(gram)
  rf <- round(table(gram)/length(gram), 8)
  tf <- as.data.frame(rf, row.names = NULL, stringsAsFactors = F)
  tf$abs <- f
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
  
  idx <- grep(sw, tf$gram, perl = TRUE)
  if (length(idx) == 0) tf.s <- tf
  if (length(idx) > 0) tf.s <- tf[-idx, ]
  rm(tf);  rm(sw);

  #Indexing. For bigrams, only take [1] the first word to index
  #for n>2, need to combine 1 - n-1 into a single word index
  print(paste(timestamp(quiet = T), "index unlisting"))
  t <- unlist(lapply(strsplit(tf.s$gram, " "), function(x) x[1:n-1]))
  print(paste(timestamp(quiet = T), "index subsetting"))
  #needs to be on unlisted and 1 big blob of text
  p <- t[seq(1, length(t), by = n - 1)]
  #process additional grams if n > 2
  print(paste(timestamp(quiet = T), "index additional grams"))
  if (n > 2)  for(j in 2:(n - 1))  p <- paste(p, t[seq(j, length(t), by = n - 1)])
  tf.s$idx <- p
  
  #up to the above line can 

  print(paste(timestamp(quiet = T), "lookup"))
  #only keep the most frequent ngram by index
  lookup <- setNames(aggregate(tf.s$Freq, by = list(tf.s$idx), max), c("idx", "Freq"))
  #here look at filtering out grams with freq == 1
  
  print(paste(timestamp(quiet = T), "create model"))
  model <- merge(lookup, tf.s)
  #remove duplicate idx and freq, taking the first (alpha) occurance
  #only take freq or rel freq in next step, depending on final implementation
  print(paste(timestamp(quiet = T), "model remove dups"))
  model <- model[!duplicated(model$idx), ]
  #remove first n-1 grams and keep only the term to predict
  print(paste(timestamp(quiet = T), "model unlisting"))
  model$gram <- unlist(lapply(strsplit(model$gram, " "), function(x) unclass(x)[n]))
  rm(gram); rm(tf.s); rm(lookup)
  
  model
}
 
combineModels <- function(model){
  print(paste(timestamp(quiet = T), "rbind list"))
  model <- rbind.fill(model)
  
  print(paste(timestamp(quiet = T), "aggregate"))
  lookup <- setNames(aggregate(model$abs, by = list(model$idx), max), c("idx", "maxAbs"))
  
  print(paste(timestamp(quiet = T), "merge final"))
  final <- merge(lookup, model, by.x = c("idx", "maxAbs"), by.y = c("idx", "abs"))
 
  print(paste(timestamp(quiet = T), "model remove dups"))
  final <- final[!duplicated(final$idx), ]
  
  #add freq in the return
  #final[, c("idx", "x")]
  final
}

singleModel <- function(ngrams){
  #takes a list of multiple ngrams and combines them before computing frequency
  
}

bigram.model.samp <- createModel(bigram.samp.100000, 2)
trigram.model.samp <- createModel(trigram.samp.100000, 3)
quad.model.samp <- createModel(quadgram.samp.100000, 4)

stupidBackoff <- function(phrase){
  phrase <- strsplit(phrase, " ")[[1]]
  len <- length(phrase)
  bi <- NULL; tri <- NULL; quad <- NULL;
  if (len >= 1) 
    bi <- bigram.model.samp[bigram.model.samp$idx == 
                              paste(phrase[len], collapse = " "), ]
  if (len >= 2)
    tri <- trigram.model.samp[trigram.model.samp$idx == 
                                paste(phrase[(len - 1) : len], collapse = " "), ]
  if (len >= 3)
    quad <- quad.model.samp[quad.model.samp$idx == 
                              paste(phrase[(len - 2) : len], collapse = " "), ]
  
  ifelse (length(quad$gram) > 0, quad$gram, 
          ifelse (length(tri$gram) > 0, tri$gram, 
                  ifelse (length(bi$gram) > 0, bi$gram,
                          #                     #Implement POS tagger
                          "the"
                  )))
}

cleanInput <- function(phrase){
  paste(gsub("'", "", tokenize(toLower(phrase), removePunct = T, removeSeparators = T, 
                               concatenator = " ", ngrams = 1)[[1]], fixed = T), collapse = " ")
}

#in sample test set
r <- readRDS("data/readLines_list_all_EN.RDS")
is <- unlist(lapply(r, sample, 1000))
saveRDS(is, "data/test_3000.RDS")
ist <- lapply(strsplit(is, ' '), function(x) paste(x[1 : (length(x)-1)], collapse = " "))
isa <- sapply(strsplit(is, ' '), function(x) paste(x[length(x)], collapse = " "))
isa <- tokenize(toLower(isa), removePunct = T, removeSeparators = T)

#out of sample test set
npr.text <- NULL
for (i in 0:11){
  url2 <- paste0("http://api.npr.org/query?id=1007,3&numResults=20&output=JSON&startNum=", (20*i)+1,
                 "&apiKey=", readLines("test/npr.key"))
  npr.json <- fromJSON(url2)
  for (j in 1: length(npr.json$list$story$text$paragraph)){
    if(nrow(npr.json$list$story$text$paragraph[[j]]) > 1){
      u <- unclass(npr.json$list$story$text$paragraph[[j]]$`$text`)
      npr.text [[220 + j]] <- u[1:(length(u) -1)]
    }
  }
}
npr.text <- unlist(npr.text)[1:3000]
npr.text <- npr.text[-correct] #filter out initial run of 30 matches which were badly formed lines
ost <- lapply(strsplit(npr.text, ' '), function(x) paste(x[1 : (length(x)-1)], collapse = " "))
osa <- sapply(strsplit(npr.text, ' '), function(x) paste(x[length(x)], collapse = " "))
osa <- tokenize(toLower(osa), removePunct = T, removeSeparators = T)

#predict and compare
pred <- lapply(ist, stupidBackoff)
comp <- NULL
for(i in 1 : length(pred))  comp <- rbind(comp, c(isa[[i]], pred[[i]]))
c(paste0(sum(comp[,1] == comp[,2]), "/", nrow(comp)), 
  paste0(round(sum(comp[,1] == comp[,2])/nrow(comp)*100, 4), "%"))

#consider using unigrams from news dataset as a dictionary. combine that with a frequency threshold
#so things high frequency internet slang doesn't get filtered out. 

#convert @ to "at", other misc cleanup, ie "**text**

#consider data.tables or sqldf for faster search into models. 
#need unigram freq (to use for parts of speech)
