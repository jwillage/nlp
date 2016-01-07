library(openNLP)
library(NLP)

#bigram.model.sing <- readRDS("model_bi_combined.RDS")
#trigram.model.sing <- readRDS("model_tri_combined.RDS")
#quadgram.model.sing <- readRDS("model_quad_combined.RDS")
bigram.model.sing <- readRDS("model_bi_combined_hash.RDS")
trigram.model.sing <- readRDS("model_tri_combined_hash.RDS")
quadgram.model.sing <- readRDS("model_quad_combined_hash.RDS")
pos <- readRDS("pos.RDS")

sta <- Maxent_Sent_Token_Annotator()
wta <- Maxent_Word_Token_Annotator()
pta <- Maxent_POS_Tag_Annotator()

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
    bi <- bigram.model.sing[bigram.model.sing$idx == 
                              hash(paste(phrase[len], collapse = " ")), ]
  if (len >= 2)
    tri <- trigram.model.sing[trigram.model.sing$idx == 
                                hash(paste(phrase[(len - 1) : len], collapse = " ")), ]
  if (len >= 3)
    quad <- quadgram.model.sing[quadgram.model.sing$idx == 
                              hash(paste(phrase[(len - 2) : len], collapse = " ")), ]

  ifelse (c(length(quad$gram) > 0, length(quad$gram) > 0), c(quad$gram, 4), 
          ifelse (c(length(tri$gram) > 0, length(tri$gram) > 0), c(tri$gram, 3), 
                  ifelse (c(length(bi$gram) > 0, length(bi$gram) > 0), c(bi$gram, 2),
                        #  c("the", 1) ))) #POS tagger
                        ifelse(len > 0, c(
                          pos[pos$pos == annotate(phrase[len], pta, annotate(phrase[len], 
                           list(sta, wta)))[2]$features[[1]][[1]], "pred"], 1), c("", 1))) ))
}

stupidBackoffa <- function(phrase, bi.model, tri.model, quad.model){
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
                  )))
}

