library(openNLP)
library(NLP)
library(plyr)
library(dplyr)
library(hashr)

bi.model <- readRDS("model_bi_combined_top5_hash.RDS")
tri.model <- readRDS("model_tri_combined_top5_hash.RDS")
quad.model <- readRDS("model_quad_combined_top5_hash.RDS")
quint.model <- readRDS("model_quint_combined_top5_hash.RDS")
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

stupidBackoff <- function(phrase, hash = F, top = 1, bi.model = model.bi.k5, tri.model = model.tri.k5, 
                          quad.model = model.quad.k5, quint.model = model.quint.k5){
  phrase <- strsplit(phrase, " ")[[1]]; p <- NULL
  len <- length(phrase)
  bi <- data.frame(); tri <- data.frame(); quad <- data.frame(); quint <- data.frame();

  if (len >= 1){
    p <- paste(phrase[len], collapse = " ")
    bi <- bi.model[bi.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(bi) > 0) bi$n <- 2}
  if (len >= 2){
    p <- paste(phrase[(len - 1) : len], collapse = " ")
    tri <- tri.model[tri.model$idx == ifelse(hash, hash(p), p), ]
    if (nrow(tri) >0) tri$n <- 3}
  if (len >= 3){
    p <- paste(phrase[(len - 2) : len], collapse = " ")
    quad <- quad.model[quad.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(quad) >0)  quad$n <- 4}
  if (len >= 4){
    p <- paste(phrase[(len - 3) : len], collapse = " ")
    quint <- quint.model[quint.model$idx ==  ifelse(hash, hash(p), p), ]
    if (nrow(quint) >0) quint$n <- 5}
  
  ret <- NULL
  ret <- rbind(quint, quad, tri, bi)
  ret <- ret[!duplicated(ret$gram),]
  if (nrow(ret) > 0)
    return(ret[order(ret$n,ret$Freq, decreasing = T),][1:top,])

  ifelse(length(phrase) > 0, 
    ret<-data.frame(idx = "POS", gram = pos[pos$pos == annotate(phrase[len], pta, annotate(phrase[len], 
                                                       list(sta, wta)))[2]$features[[1]][[1]], "pred"], 
               freq = 0, n = 1, stringsAsFactors = F), 
    ret<-data.frame(idx = "", gram = "", freq = 0,n = 1, stringsAsFactors = F))
  ret
}

#consider including weighted top trigram etc options that do not match with quadgram 
#have k as a parm. ie only take into account the first k matches and work backwards from there.
simpleInterpolation <- function(phrase, lambda = 0.4, top = 1, hash = F, bi.model = model.bi.k5, 
                                tri.model = model.tri.k5, quad.model = model.quad.k5, 
                                quint.model = model.quint.k5){
  score <- function(modelResult, modelList){
    #unigram list is mandatory. further interpolation option but models need to be in asc n order
    
    gram <- modelResult$gram
    scores <- list(); ret <- list();
    
    scores[[1]] <- modelList[[1]][modelList[[1]]$gram %in% gram, "Freq"] 
    scores[[1]] <- data.frame(Freq = scores[[1]], gram = names(scores[[1]]), 
                             scores = scores[[1]] * lambda^(length(modelList)), 
                             stringsAsFactors = F)
    
    if(length(modelList) > 1)
      for(i in 2 : (length(modelList))){
        p <- paste(phrase[(len-(i-2)):len], collapse = " ")
        scores[[length(scores) + 1]] <- modelList[[i]][modelList[[i]]$gram %in% gram & 
                                                       modelList[[i]]$idx == ifelse(hash, hash(p), p), 
                                                     c("Freq", "gram")]
        scores[[length(scores)]]$scores <- scores[[length(scores)]]$Freq * 
          (lambda^(length(modelList)+1-i))
      }
    
    scores <- rbind.fill(scores)
    scores <- rbind(scores, 
          data.frame(Freq = modelResult$Freq, gram = modelResult$gram, scores = modelResult$Freq))
    scores <- setNames(aggregate(scores$scores, by = list(scores$gram), sum), c("gram", "scores"))
    scores[order(scores$scores, decreasing = T), ][1:top,]
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
  #add any supplmental info ie score, which gram won, etc.
  ifelse(!is.null(quint),
    ret <- cbind(score(quint, list(unigrams, bi.model, tri.model, quad.model)), 5),
    ifelse(!is.null(quad),
           ret <- cbind(score(quad, list(unigrams, bi.model, tri.model)), 4),
           ifelse(!is.null(tri),
              ret <- cbind(score(tri, list(unigrams, bi.model)), 3),
              ifelse(!is.null(bi),
                 ret <- cbind(score(bi, list(unigrams)), 2),
                 ifelse(len > 0, {
                   gram <- pos[pos$pos == annotate(phrase[len], pta, annotate(phrase[len], 
                              list(sta, wta)))[2]$features[[1]][[1]], "pred"]
                   if(length(gram) == 0) gram <- "<NA>"
                   ret <- data.frame(gram = gram
                   , scores = NA, n = 1, stringsAsFactors = F)}, 
                   ret <- data.frame(gram = "", scores = NA, n = 1, stringsAsFactors = F))
            ) ) ) )
  names(ret)[3] <- "n"
  ret
}