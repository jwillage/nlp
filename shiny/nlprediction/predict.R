bigram.model.samp <- readRDS("model_bi_sample_100k_EN_new.RDS")
trigram.model.samp <- readRDS("model_tri_sample_100k_EN_new.RDS")
quad.model.samp <- readRDS("model_quad_sample_100k_EN_new.RDS")
library(openNLP)
#library(NLP)

cleanInput <- function(phrase){
  library(quanteda)
  paste(gsub("'", "", tokenize(toLower(phrase), removePunct = T, removeSeparators = T, 
                         concatenator = " ", ngrams = 1)[[1]], fixed = T), collapse = " ")
}

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

  ifelse (c(length(quad$gram) > 0, length(quad$gram) > 0), c(quad$gram, 4), 
          ifelse (c(length(tri$gram) > 0, length(tri$gram) > 0), c(tri$gram, 3), 
                  ifelse (c(length(bi$gram) > 0, length(bi$gram) > 0), c(bi$gram, 2),
                          #                     #Implement POS tagger
                          c("the", 1)
                  )))
}

stupidBackoff <- function(phrase, bi.model, tri.model, quad.model){
  phrase <- strsplit(phrase, " ")[[1]]
  len <- length(phrase)
  bi <- NULL; tri <- NULL; quad <- NULL;
  if (len >= 1) 
    bi <- bi.model[bi.model$idx ==  paste(phrase[len], collapse = " "), ]
  if (len >= 2)
    tri <- tri.model[tri.model$idx == paste(phrase[(len - 1) : len], collapse = " "), ]
  if (len >= 3)
    quad <- quad.model[quad.model$idx ==  paste(phrase[(len - 2) : len], collapse = " "), ]
  
  ifelse (c(length(quad$gram) > 0, length(quad$gram) > 0), c(quad$gram, 4), 
          ifelse (c(length(tri$gram) > 0, length(tri$gram) > 0), c(tri$gram, 3), 
                  ifelse (c(length(bi$gram) > 0, length(bi$gram) > 0), c(bi$gram, 2),
                          #                     #Implement POS tagger
                          c("the", 1)
                        #  annotate(phrase[len], pos_tag_annotator, annotate(phrase[len], 
                         # list(sent_token_annotator, word_token_annotator)))[2]$features[[1]][[1]]
                  )))
}

