bigram.model.samp <- readRDS("model_bi_sample_100k_EN_new.RDS")
trigram.model.samp <- readRDS("model_tri_sample_100k_EN_new.RDS")
quad.model.samp <- readRDS("model_quad_sample_100k_EN_new.RDS")

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

  ifelse (length(quad$gram) > 0, quad$gram, 
          ifelse (length(tri$gram) > 0, tri$gram, 
                  ifelse (length(bi$gram) > 0, bi$gram,
                          #                     #Implement POS tagger
                          "the"
                  )))
}