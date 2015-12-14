library(tm)
library(Matrix)

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

lapply(sapply(en, nchar), max)
#the longest document is in the blogs data set, over 40K characters

corp <- lapply(en, corpus)
rm(en)

#when ready, combine into single corpus
#t <- c(corp.samp[[1]], corp.samp[[2]], corp.samp[[3]])

#tokenize and remove stopwords, etc.
#we make the decision to remove punctuation, numbers, whitepsace, stopwords, change to lower

# a simple vector version in case we can use in the future. much smaller.
#tok <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
#                                                 removeSeparators = T, simplify = T )))
tok.list <- lapply(corp, function(x) toLower(tokenize(x, removeNumbers = T, removePunct = T, 
                                                 removeSeparators = T)))
tok.list.s <- lapply(tok.list, function(x) removeFeatures(x, readLines("stop.txt")))
rm(tok.list)


dfm.en <- lapply(t, dfm)

#think about removing terms that only appear once?
#try removeSparseTerms(bigTDM, sparse= 0.8)


#tokenize
#MC_tokenize each document in a corpus. lapply that over each of the corpora. 
tok <- lapply(corp.samp.mod, function(x) sapply(x, function(y) MC_tokenizer(y)))
#to return 1 large token list of a single corpus
token.corpus <- lapply(tok, function(x) unlist(x))

#frequency exploration
tdm <-lapply(corp.samp.mod, function(x) TermDocumentMatrix(x))
sm <- lapply(tdm, function(x) sparseMatrix(i=x$i, j=x$j, x=x$v))
freq <- list()
for (i in 1:length(sm))
  freq[[length(freq) +1]] <- data.frame(term = tdm[[i]]$dimnames$Terms, freq = rowSums(sm[[i]]))

#view 50 most frequent tokens
freq.sort <- lapply(freq, function(x) x[order(x$freq, decreasing = T), ])
cbind(freq.sort[[1]][1:50,], freq.sort[[2]][1:50,], freq.sort[[3]][1:50,])

#mapply(cbind, freq[[1]]$freq[1:50], freq[[2]]$freq[1:50], freq[[3]]$freq[1:50])
#do.call(mapply, c(cbind,  freq))

#we see the top 50 tokens for twitter, news, blogs, respectively
#twitter seems to be a lot more diversified, with only two terms from the sample appearing > 100x
#compare how conversational twitter is, with most freq term being "you" (also see "your" at no. 3)
#that appears much lower in news, and blogs somehwere in between. 

