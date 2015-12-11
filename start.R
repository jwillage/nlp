library(tm)
library(Matrix)

fn <- "data/Coursera-SwiftKey.zip"
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                     , fn)
l <- unzip(fn, list = T)

# #work with text documents in list format
# tmp <- list()
# for (i in l$Name[grep("US.*txt", l$Name)])
#   tmp[[length(tmp) + 1]] <- readLines(unz("data/Coursera-SwiftKey.zip", i))
# tmp <- setNames(tmp, gsub(".txt", "" ,gsub(".*/.*/", "", l$Name[grep("txt", l$Name)])))

#use dbConrol parm to create a database on disk instead of storing everything in memory

# #some functions available with tm
# stemDocument(c[[1]])$content
# inspect(crudeTDM)[crudeTDMhf, 1:10]
# findAssocs(crudeTDM, "saudi", .8)

zs <- ZipSource(fn, pattern = "US.*txt", recursive = T)
corp <- Corpus(zs, readerControl = list(reader = readPlain,  language = "en_US", load = TRUE))

corp.samp <- list()
for (i in l$Name[grep("US.*txt", l$Name)]){
  df.source <- DataframeSource(data.frame(c= readLines(unz("data/Coursera-SwiftKey.zip", i), 1000)))
  corp.samp[[length(corp.samp) + 1]] <- Corpus(df.source, readerControl = 
                                                 list(reader = readPlain,  language = "en_US", 
                                                      load = TRUE))
}

#when ready, combine into single corpus
#t <- c(corp.samp[[1]], corp.samp[[2]], corp.samp[[3]])

#remove stop words
sw <- readLines("stop.txt")
rw <- function(x) removeWords(x, sw)
#we make the decision to remove punctuation, numbers, whitepsace, stopwords, change to lower
funs <- list(stripWhitespace, rw, content_transformer(tolower), removePunctuation, removeNumbers)
corp.samp.mod <- lapply(corp.samp, function(x) tm_map(x, FUN = tm_reduce, tmFuns = funs))

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
#compare how conversation twitter is, with most freq term being "you" (also see "your" at no. 3)
#that appears much lower in news, and blogs somehwere in between. 


