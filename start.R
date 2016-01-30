library(plyr)
library(quanteda)
library(jsonlite)
library(data.table)
library(openNLP)
library(NLP)

createModel <- function(tok, n, k = 1){
  #    Builds a frequency-based model from a list of n-grams
  #    
  #    Args:
  #      tok:  A list of tokenized text. Can accept a list of lists, ie multiple corpora in different
  #            slots.
  #      n:    Length of the grams, the n in the n-gram.
  #      k:    Number of grams to keep per index. 
  #      
  #    Returns:
  #      A lookup table model containing the index, predicted gram, and frequency of the gram in the 
  #      input corpus
  #
  #    TODO:
  #      Implement OOV - replace low freq words with <unk> token
  
  tok <- unlist(tok)
  len <- length(tok)
  tok <- gsub("'", "", unlist(tok), fixed = TRUE)
  
  # Precision needs to increase from 8 as tok grows. Needs to capture precision up to 1/length(tok)
  freq <- table(tok)
  df <- as.data.table(round(freq/len, 8), row.names = NULL, stringsAsFactors = FALSE)
  df$absolute <- freq
  rm(freq)
  
  # Remove grams with words that appear in the profrane list
  link <- paste0("https://github.com/shutterstock/",
                 "List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/raw/master/en")
  sw <- paste0("(([^a-z]+|^)(", paste(readLines(link), sep = "", collapse = "|"), ")([^a-z]+|$))")
  # Remove grams with profanity within the word
  sw <- paste0(sw, "|", paste0(readLines("stop.txt"), sep = "", collapse = "|"))
  # Remove grams with numbers, #hashtags, @mentions, and remaining special characters
  sw <- paste0(sw, "|[0-9]|[#@_]")
  
  bad <- grep(sw, df$tok, perl = TRUE)
  if(length(bad) > 0 ) df <- df[-bad, ]
  
  split <- strsplit(df$tok, " ")
  df$idx <- sapply(split, function(x) paste(x[1:n - 1], collapse = " "))
  df$gram <- sapply(split, function(x) x[n])
  
  # Keep top k predictions
  model <- df[df[, .I[order(N, decreasing = TRUE)[1:k]], by = idx]$V1, 
              c("idx", "gram", "N", "absolute"), with = FALSE]
  
  model[complete.cases(model), ]
}

combineModels <- function(model, k = 1){
  #    Aggregates multiple models, useful for building models over different corpora
  #    
  #    Args:
  #      model: A list of models
  #      k:     Number of grams to keep per index. 
  #
  #    Returns:
  #      A model in the same structure as those passed in, aggregated by top k grams per index
  
  model <- as.data.table(rbind.fill(model))
  merged <- model[model[, .I[order(N, decreasing = TRUE)[1:k]], by = idx]$V1, 
                  c("idx", "gram", "N", "absolute"), with = FALSE]
  
  merged[complete.cases(merged), ]
}

posModel <- function(model.bi){
  #    Create a model whose indices are parts of speech and includes the next most likely word
  #    
  #    Args:
  #      model.bi: Bigram model
  #
  #    Returns:
  #      A data.table model indexed by POS, containing most likely next word
  
  sta <- Maxent_Sent_Token_Annotator()
  wta <- Maxent_Word_Token_Annotator()
  pta <- Maxent_POS_Tag_Annotator()
  
  # Resource problems when running in entirety. Break up into ~100k chunks
  y1 <- annotate(model.bi$idx, list(sta, wta))
  y2 <- annotate(model.bi$idx, pta, y1)
  y2w <- subset(y2, type == "word")
  model.bi$pos <- sapply(y2w$features, '[[', "POS")
  lookup <- as.data.table(aggregate(model.bi$abs, by = list(model.bi$pos, model.bi$gram), sum))
  model <- lookup[lookup[, .I[order(x, decreasing = TRUE)[1]], by = Group.1]$V1, 
                      c("Group.1", "Group.2"), with = FALSE]
  setnames(model, c("pos", "pred"))
  
  model
}

createSample <- function(samp){
  #    Create a set of tests and answers to evaluate prediction algorithms
  #    
  #    Args:
  #      samp:  a character array of phrases to turn into tests
  #
  #    Returns:
  #      A two element list containing a character vector of tests and a character vector of answers
  
  ret <- list()
  ret[["test"]] <- sapply(strsplit(samp, ' '), 
                              function(x) paste(x[1:(length(x) - 1)], collapse = " "))
  ret[["answer"]] <- sapply(strsplit(samp, ' '), 
                                function(x) paste(x[length(x)], collapse = " "))
  ret[["answer"]] <- as.character(tokenize(toLower(ret[[2]]), removePunct = TRUE, 
                                  removeSeparators = TRUE))
  
  na <- which(ret[["answer"]] == 'character(0)')
  if (length(na) > 0){
    ret[["test"]] <- ret[["test"]][-na]
    ret[["answer"]] <- ret[["answer"]][-na]
  }
  
  ret
}

getNPRText <- function(numResults, key){
  #    Make call to NPR's Story API to retrieve paragraphs of text
  #    
  #    Args:
  #      numResults:  Total number of paragraphs to pull back
  #      key:         Private NPR API key
  #
  #    Returns:
  #      A character vector of paragraphs from NPR stories
  
  npr.text <- NULL
  url1 <- paste0("http://api.npr.org/query?id=1007,3&numResults=20&output=JSON&apiKey=", key)
  # NPR API max results = 20
  i <- 0
  while(length(npr.text) < numResults){
    url2 <- paste0(url1, "&startNum=", (20 * i) + 1)
    npr.json <- fromJSON(url2)
    for (j in 1:length(npr.json$list$story$text$paragraph)){
      if(nrow(npr.json$list$story$text$paragraph[[j]]) < 2) next()
      u <- unclass(npr.json$list$story$text$paragraph[[j]]$`$text`)
      npr.text <- append(npr.text, u[1:length(u) - 1])
    }
    i <- i + 1
  }
  
  npr.text[1:numResults]
}

testAlgorithm <- function(samp, alg, ...){
  #    Test algorithm and compare results to samples
  #    
  #    Args:
  #      samp: Output of createSample, or similar structure (a 2 element list containing tests and 
  #            answers, respectively)
  #      alg:  Vectorized function containing algorithm
  #      ...:  Further arguments passed to algorithm
  #
  #    Returns:
  #      The percentage of answers the algorithm correctly predicted

  pred <- alg(cleanInput(samp[[1]]), ...)
  pred <- unlist(t(pred)[, 2])
  
  comp <- cbind(samp[[2]], pred)
  c(paste0(sum(comp[, 1] == comp[, 2]), "/", nrow(comp)), 
    paste0(round(sum(comp[, 1] == comp[, 2])/nrow(comp) * 100, 4), "%"))
}

fn <- "data/Coursera-SwiftKey.zip"
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
              , fn)
l <- unzip(fn, list = TRUE)

tmp <- list()
for (i in l$Name)
  tmp[[length(tmp) + 1]] <- readLines(unz("data/Coursera-SwiftKey.zip", i))
tmp <- setNames(tmp, gsub(".txt", "" , gsub(".*/.*/", "", l$Name[grep("txt", l$Name)])))

en <- list()
en[["twitter"]] <- tmp[[7]]; en[["news"]] <- tmp[[8]]; en[["blogs"]] <- tmp[[9]];
rm(tmp)

#news sentences are broken up by period. blogs and twitter have less reliable breaks
en[["news"]] <- unlist(stri_split_boundaries(en[["news"]], type = "sentence", 
                                             skip_sentence_sep = TRUE))
corp <- lapply(en, corpus)
unigram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = TRUE, 
                                             removeSeparators = TRUE, 
                                             concatenator = " ", ngrams = 1))
bigram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = TRUE, 
                                            removeSeparators = TRUE, 
                                            concatenator = " ", ngrams = 2))
# ...
quintgram <- lapply(corp, function(x) tokenize(toLower(x), removePunct = TRUE, 
                                               removeSeparators = TRUE, 
                                               concatenator = " ", ngrams = 5))

model.bi <- createModel(bigram, n = 2, k = 5)
model.bi.hash <- model.bi
model.bi.hash$idx <- as.integer(hashr::hash(model.bi.hash$idx))
pos <- posModel(model.bi)

samp.in <- createSample(unlist(lapply(en, sample, 1000)))
samp.out <- createSample(getNPRText(1, readLines("test/npr.key")))

cleanInput <- Vectorize(cleanInput)
mods <- list(bi.model.hash, tri.model.hash, quad.model.hash, quint.model.hash)
testAlgorithm(samp.out, Vectorize(stupidBackoffFixed), hash = TRUE)
