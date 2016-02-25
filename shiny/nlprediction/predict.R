library(ggplot2)
library(ggthemes)
library(openNLP)
library(NLP)
library(plyr)
library(dplyr)
library(hashr)
library(quanteda)

bi.model.hash <- readRDS("../../model/model_bi_combined_top5_hash.RDS")
tri.model.hash <- readRDS("../../model/model_tri_combined_top5_hash.RDS")
quad.model.hash <- readRDS("../../model/model_quad_combined_top5_hash.RDS")
quint.model.hash <- readRDS("../../model/model_quint_combined_top5_hash.RDS")
unigrams <- readRDS("../../data/unigram_tf.s.RDS")
speech <- readRDS("../../model/pos.RDS")

sta <- Maxent_Sent_Token_Annotator()
wta <- Maxent_Word_Token_Annotator()
pta <- Maxent_POS_Tag_Annotator()

cleanInput <- function(phrase){
  #    Clean user input to match model
  #    
  #    Args:
  #      phrase: Character vector of raw user input
  #      
  #    Returns:
  #      A cleaned character vector, with punctuation, whitespace, and apostrophes removed
  
  paste(gsub("'", "", tokenize(toLower(phrase), removePunct = TRUE, removeSeparators = TRUE, 
                               concatenator = " ", ngrams = 1)[[1]], fixed = TRUE), collapse = " ")
}

stupidBackoff <- function(phrase, modelList, hash = FALSE, top = 1){
  #    Run Stupid Backoff algorithm on input phrase
  #    
  #    Args:
  #      phrase:    Character vector of user input. Should be run through any preprocessing first.
  #      modelList: A list of n-gram models, in ascending n order
  #      hash:      Whether or not the models in modelList are hashed
  #      top:       Number indicating how many results to return
  #      
  #    Returns:
  #      A data frame containing the predicted word
  
  phrase <- strsplit(phrase, " ")[[1]]
  p <- NULL
  len <- length(phrase)
  grams <- replicate(length(modelList), data.frame())
  
  grams <- lapply(1:length(modelList), function(i) {
      p <- paste(phrase[(len - (i - 1)):len], collapse = " ")
      a <- modelList[[i]][modelList[[i]]$idx == ifelse(hash, hash(p), p), ]
      if (nrow(a) > 0)
        a$n <- i + 1
      a
  })
  
  ret <- rbind.fill(grams)
  ret <- ret[!duplicated(ret$gram), ]
  ret$scores <- ret$Freq
  if (nrow(ret) > 0)
    return(ret[order(ret$n, ret$Freq, decreasing = TRUE), ][1:top, ])
  
  ifelse(length(phrase) > 0, 
         ret <- data.frame(idx = "POS", 
                           gram = speech[speech$pos == NLP::annotate(phrase[len], pta, 
                                                               NLP::annotate(phrase[len], 
                                                                             list(sta, wta)))[2]
                                                       $features[[1]][[1]], 
                                        "pred"], 
                           freq = 0, n = 1, scores = 0, stringsAsFactors = FALSE), 
         ret <- data.frame(idx = "", gram = "", freq = 0, n = 1, scores = 0, stringsAsFactors = FALSE)
  )
  
  ret
}

simpleInterpolation <- function(phrase, lambda = 0.4, top = 1, hash = F, bi.model = model.bi.k5, 
                                tri.model = model.tri.k5, quad.model = model.quad.k5, 
                                quint.model = model.quint.k5){
  #    Predicts next most likely word using simple interpolation. Lower order predictions are
  #    fixed to match the highest order n-grams.
  #    
  #    Args:
  #      phrase:    Character vector of user input. Should be run through any preprocessing first.
  #      lambda:    lambda factor to multiple each level of depth
  #      top:       Number indicating how many results to return
  #      hash:      Whether or not the models in modelList are hashed
  #      bi.model:  bigram model, etc...
  #      
  #    Returns:
  #      A data frame of up to the top k next-word predictions 
  #
  #    TODO:
  #     Combine if chains from both predict functions
  
  score <- function(modelResult, modelList){
    # Unigram list is mandatory as first model. Additional models need to be in asc n order
    gram <- modelResult$gram
    scores <- list()
    ret <- list()
    
    scores[[1]] <- modelList[[1]][modelList[[1]]$gram %in% gram, "Freq"] 
    scores[[1]] <- data.frame(Freq = scores[[1]], gram = names(scores[[1]]), 
                              scores = scores[[1]] * lambda ^ (length(modelList)), 
                              stringsAsFactors = FALSE)
    
    if(length(modelList) > 1)
      for(i in 2 : (length(modelList))){
        p <- paste(phrase[(len - (i - 2)):len], collapse = " ")
        scores[[length(scores) + 1]] <- modelList[[i]][modelList[[i]]$gram %in% gram & 
                                                    modelList[[i]]$idx == ifelse(hash, hash(p), p), 
                                                       c("Freq", "gram")]
        scores[[length(scores)]]$scores <- scores[[length(scores)]]$Freq * 
          (lambda ^ (length(modelList) + 1 - i))
      }
    
    scores <- rbind.fill(scores)
    scores <- rbind(scores, 
                    data.frame(Freq = modelResult$Freq, gram = modelResult$gram, 
                               scores = modelResult$Freq))
    scores <- setNames(aggregate(scores$scores, by = list(scores$gram), sum), c("gram", "scores"))
    scores[order(scores$scores, decreasing = TRUE), ][1:top, ]
  }
  
  phrase <- strsplit(phrase, " ")[[1]]
  len <- length(phrase)
  bi <- NULL; tri <- NULL; quad <- NULL; quint <- NULL;
  if (len >= 1){ 
    p <- paste(phrase[len], collapse = " ")
    bi <- bi.model[bi.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(bi) == 0) bi <- NULL
  }
  if (len >= 2){
    p <- paste(phrase[(len - 1) : len], collapse = " ")
    tri <- tri.model[tri.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(tri) == 0) tri <- NULL 
  }
  if (len >= 3){
    p <- paste(phrase[(len - 2) : len], collapse = " ")
    quad <- quad.model[quad.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(quad) == 0) quad <- NULL
  }
  if (len >= 4){
    p <- paste(phrase[(len - 3) : len], collapse = " ")
    quint<- quint.model[quint.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(quint) == 0) quint <- NULL
  }
  
  ret <- NULL
  ifelse(!is.null(quint),
         ret <- cbind(score(quint, list(unigrams, bi.model, tri.model, quad.model)), 5),
         ifelse(!is.null(quad),
                ret <- cbind(score(quad, list(unigrams, bi.model, tri.model)), 4),
                ifelse(!is.null(tri),
                       ret <- cbind(score(tri, list(unigrams, bi.model)), 3),
                       ifelse(!is.null(bi),
                              ret <- cbind(score(bi, list(unigrams)), 2),
                              ifelse(len > 0, {
                                gram <- speech[speech$pos == NLP::annotate(phrase[len], pta, 
                                                                     NLP::annotate(phrase[len],
                                                             list(sta, wta)))[2]$features[[1]][[1]],
                                            "pred"]
                                if(length(gram) == 0) gram <- "<NA>"
                                ret <- data.frame(gram = gram
                                                  , scores = 0, n = 1, stringsAsFactors = FALSE)}, 
                                ret <- data.frame(gram = "", scores = 0, n = 1, 
                                                  stringsAsFactors = FALSE))
                       ) ) ) )
  names(ret)[3] <- "n"

  ret
}

simpleInterpolation2 <- function(phrase, lambda = 0.4, top = 1, hash = F, bi.model = model.bi.k5, 
                                 tri.model = model.tri.k5, quad.model = model.quad.k5, 
                                 quint.model = model.quint.k5){
  #    Predicts next most likely word using free interpolation, lower order n-gram possibilities
  #    not dependent on highest order n-gram
  #    
  #    Args:
  #      phrase:    Character vector of user input. Should be run through any preprocessing first.
  #      lambda:    lambda factor to multiple each level of depth
  #      top:       Number indicating how many results to return
  #      hash:      Whether or not the models in modelList are hashed
  #      bi.model:  bigram model, etc...
  #      
  #    Returns:
  #      A data frame of up to the top k next-word predictions 
  
    score <- function(modelResult, modelList){
    
    gram <- modelResult$gram
    scores <- list()
    ret <- list()
    
    scores[[1]] <- modelList[[1]][order(modelList[[1]]$abs, decreasing = TRUE), ][1:5, ]
    scores[[1]] <- data.frame(Freq = scores[[1]]$Freq, gram = scores[[1]]$gram, 
                              scores = scores[[1]]$Freq * lambda ^ ((length(modelList)) + 1), 
                              stringsAsFactors = FALSE)

    if (length(modelList) > 1)
      for (i in 2 : (length(modelList))){
        p <- paste(phrase[(len - (i - 2)):len], collapse = " ")
        scores[[length(scores) + 1]] <- modelList[[i]][modelList[[i]]$idx == 
                                                         ifelse(hash, hash(p), p), 
                                                       c("Freq", "gram")]
        scores[[length(scores)]]$scores <- scores[[length(scores)]]$Freq * 
          (lambda ^ (length(modelList) + 1 - i))
      }
    
    scores[[length(scores) + 1]] <- data.frame(Freq = modelResult$Freq, gram = modelResult$gram, 
                                               scores = modelResult$Freq)
    scores <- rbind.fill(scores)
    scores <- setNames(aggregate(scores$scores, by = list(scores$gram), sum), c("gram", "scores"))
    scores[order(scores$scores, decreasing = TRUE), ][1:top, ]
  }
  
  phrase <- strsplit(phrase, " ")[[1]]
  len <- length(phrase)
  bi <- NULL; tri <- NULL; quad <- NULL; quint <- NULL;
  if (len >= 1){ 
    p <- paste(phrase[len], collapse = " ")
    bi <- bi.model[bi.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(bi) == 0) bi <- NULL
  }
  if (len >= 2){
    p <- paste(phrase[(len - 1) : len], collapse = " ")
    tri <- tri.model[tri.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(tri) == 0) tri <- NULL 
  }
  if (len >= 3){
    p <- paste(phrase[(len - 2) : len], collapse = " ")
    quad <- quad.model[quad.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(quad) == 0) quad <- NULL
  }
  if (len >= 4){
    p <- paste(phrase[(len - 3) : len], collapse = " ")
    quint<- quint.model[quint.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(quint) == 0) quint <- NULL
  }
  
  ret <- NULL
  ifelse(!is.null(quint),
         ret <- cbind(score(quint, list(unigrams, bi.model, tri.model, quad.model)), 5),
         ifelse(!is.null(quad),
                ret <- cbind(score(quad, list(unigrams, bi.model, tri.model)), 4),
                ifelse(!is.null(tri),
                       ret <- cbind(score(tri, list(unigrams, bi.model)), 3),
                       ifelse(!is.null(bi),
                              ret <- cbind(score(bi, list(unigrams)), 2),
                              ifelse(len > 0, {
                                gram <- speech[speech$pos == NLP::annotate(phrase[len], pta, 
                                                                     NLP::annotate(phrase[len],
                                                            list(sta, wta)))[2]$features[[1]][[1]], 
                                            "pred"]
                                if(length(gram) == 0) gram <- "<NA>"
                                ret <- data.frame(gram = gram, scores = 0, n = 1, 
                                                  stringsAsFactors = FALSE)}, 
                                ret <- data.frame(gram = "", scores = 0, n = 1, 
                                                  stringsAsFactors = FALSE))
                       ) ) ) )
  names(ret)[3] <- "n"

  ret
}

stupidBackoffFixed <- function(phrase, hash = F, top = 1, bi.model = bi.model.hash, 
                               tri.model = tri.model.hash, quad.model = quad.model.hash, 
                               quint.model = quint.model.hash, modelList){
  #    Run Stupid Backoff algorithm on input phrase. Accepts fixed list of models, and can be
  #    vectorized
  #    
  #    Args:
  #      phrase:    Character vector of user input. Should be run through any preprocessing first.
  #      modelList: A list of n-gram models, in ascending order
  #      hash:      Whether or not the models in modelList are hashed
  #      top:       Number indicating how many results to return
  #      
  #    Returns:
  #      A data frame containing the predicted word
  
  phrase <- strsplit(phrase, " ")[[1]]
  p <- NULL
  len <- length(phrase)
  bi <- data.frame(); tri <- data.frame(); quad <- data.frame(); quint <- data.frame();
  
  if (len >= 1){
    p <- paste(phrase[len], collapse = " ")
    bi <- bi.model[bi.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(bi) > 0) bi$n <- 2}
  if (len >= 2){
    p <- paste(phrase[(len - 1) : len], collapse = " ")
    tri <- tri.model[tri.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(tri) > 0) tri$n <- 3}
  if (len >= 3){
    p <- paste(phrase[(len - 2) : len], collapse = " ")
    quad <- quad.model[quad.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(quad) > 0)  quad$n <- 4}
  if (len >= 4){
    p <- paste(phrase[(len - 3) : len], collapse = " ")
    quint <- quint.model[quint.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(quint) > 0) quint$n <- 5}
  
  ret <- NULL
  ret <- rbind(quint, quad, tri, bi)
  ret <- ret[!duplicated(ret$gram), ]
  ret$scores <- ret$Freq
  if (nrow(ret) > 0)
    return(ret[order(ret$n, ret$Freq, decreasing = TRUE), ][1:top, ])
  
  ifelse(length(phrase) > 0, 
         ret <- data.frame(idx = "POS", gram = speech[speech$pos == NLP::annotate(phrase[len], pta, 
                                                                          NLP::annotate(phrase[len], 
                                                 list(sta, wta)))[2]$features[[1]][[1]], "pred"], 
                         freq = 0, n = 1, scores = 0, stringsAsFactors = FALSE), 
         ret <- data.frame(idx = "", gram = "", freq = 0, n = 1, scores = 0, 
                           stringsAsFactors = FALSE))
  
  ret
}