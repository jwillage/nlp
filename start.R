library(tm)

fn <- "data/Coursera-SwiftKey.zip"
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                     , fn)
l <- unzip(fn, list = T)
tmp <- list()
for (i in l$Name[grep("txt", l$Name)])
  tmp[[length(tmp) + 1]] <- readLines(unz("data/Coursera-SwiftKey.zip", i))

tmp <- setNames(tmp, gsub(".txt", "" ,gsub(".*/.*/", "", l$Name[grep("txt", l$Name)])))

df.source <- DataframeSource(data.frame(c= readLines(unz("data/Coursera-SwiftKey.zip", i), 100)))
c <- Corpus(df.source, readerControl = list(reader = readPlain,  language = "en_US", load = TRUE))
#creates a list of 100 text docs manually create a text doc with x <- PlainTextDocument(x = text)
#use dbConrol parm to create a database on disk instead of storing everything in memory
c[[1]]$content
#TextRepository can store multiple corpora
tdm <- TermDocumentMatrix(c)
inspect(t(tdm)[1:20, 1:10])
stemDocument(c[[1]])$content
mystopwords <- c("and", "for", "in", "is", "it", "not", "the", "to")
removeWords(c[[1]], mystopwords)$content
inspect(crudeTDM)[crudeTDMhf, 1:10]
findAssocs(crudeTDM, "saudi", .8)

zs <- ZipSource(fn, pattern = "US.*txt", recursive = T)
corp <- Corpus(zs, readerControl = list(reader = readPlain,  language = "en_US", load = TRUE))

corp.samp <- list()
for (i in l$Name[grep("US.*txt", l$Name)]){
  df.source <- DataframeSource(data.frame(c= readLines(unz("data/Coursera-SwiftKey.zip", i), 100)))
  corp.samp[[length(corp.samp) + 1]] <- Corpus(df.source, readerControl = 
                                                 list(reader = readPlain,  language = "en_US", 
                                                      load = TRUE))
}

#when ready, combine into single corpus
#a <- c(corp.samp[[1]], corp.samp[[2]], corp.samp[[3]])
