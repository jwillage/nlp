library(openNLP)
library(NLP)
library(plyr)

model.bi.k5 <- readRDS("model_bi_combined_top5_hash.RDS")
model.tri.k5 <- readRDS("model_tri_combined_top5_hash.RDS")
model.quad.k5 <- readRDS("model_quad_combined_top5_hash.RDS")
unigrams <- readRDS("unigram_tf.s.RDS")
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

#consider including weighted top trigram etc options that do not match with quadgram 
simpleInterpolation <- function(phrase, lambda = 0.4, top = 1){
  score <- function(modelResult, modelList){
    #unigram list is mandatory. further interpolation option but models need to be in asc n order
    
    gram <- modelResult$gram
    scores <- list(); ret <- list();
    
    scores[[1]] <- modelList[[1]][modelList[[1]]$gram %in% gram, "Freq"] * lambda^3
    scores[[1]] <- data.frame(Freq = scores[[1]], gram = names(scores[[1]]), 
                             scores = scores[[1]] * lambda^3)
    
    if(length(modelList) > 1)
      for(i in 2 : (length(modelList))){
        scores[[length(scores) + 1]] <- modelList[[i]][modelList[[i]]$gram %in% gram & 
                                                       modelList[[i]]$idx == 
                                                       hash(paste(phrase[(len-(i-2)):len], collapse = " ")), 
                                                     c("Freq", "gram")]
        scores[[length(scores)]]$scores <- scores[[length(scores)]]$Freq * (lambda^(4-i))
      }
    
    scores <- rbind.fill(scores)
    scores <- rbind(scores, 
          data.frame(Freq = modelResult$Freq, gram = modelResult$gram, scores = modelResult$Freq))
    scores <- setNames(aggregate(scores$scores, by = list(scores$gram), sum), c("gram", "scores"))
    scores[order(scores$scores, decreasing = T), ][1:top,]
  }
  
  phrase <- strsplit(phrase, " ")[[1]]
  len <- length(phrase)
  bi <- NULL; tri <- NULL; quad <- NULL;
  if (len >= 1){ 
    bi <- model.bi.k5[model.bi.k5$idx == hash(paste(phrase[len], collapse = " ")), ]
    if (nrow(bi) == 0) bi <- NULL
  }
  if (len >= 2){
    tri <- model.tri.k5[model.tri.k5$idx == hash(paste(phrase[(len - 1) : len], collapse = " ")), ]
    if (nrow(tri) == 0) tri <- NULL 
  }
  if (len >= 3){
    quad <- model.quad.k5[model.quad.k5$idx == hash(paste(phrase[(len - 2) : len], collapse = " ")), ]
    if (nrow(quad) == 0) quad <- NULL
  }
  
  ret <- NULL
  #add any supplmental info ie score, which gram won, etc.
  #confirm pos and default empty string are correct, ie number of times
  ifelse(rep(!is.null(quad)),
         ret <- cbind(score(quad, list(unigrams, model.bi.k5, model.tri.k5)), 4),
         ifelse(rep(!is.null(tri)),
            ret <- cbind(score(tri, list(unigrams, model.bi.k5)), 3),
            ifelse(rep(!is.null(bi)),
               ret <- cbind(score(bi, list(unigrams)), 2),
               ifelse(len > 0, ret <- data.frame(
                 gram = pos[pos$pos == annotate(phrase[len], pta, annotate(phrase[len], 
                    list(sta, wta)))[2]$features[[1]][[1]], "pred"], scores = NA, 1), 
                 ret <- data.frame(gram = "", scores = NA, 1))
            ) ) )
  ret
}

# san antonio spurs have
# Error in data.frame(Freq = scores[[1]], gram = rownames(scores[[1]]),  : 
#                       arguments imply differing number of rows: 1, 0
