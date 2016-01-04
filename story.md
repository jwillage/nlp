# Story


Multiple models were run against a set of tests. Models were built with a 100,000 *n*-grams from 
each of 3 corpora. Tests included in-sample texts and out of sample texts. In-sample included 1000
texts from each of the 3 corpora. Out-of-sample test sets were obtained from the NPR API.


```r
is <- unlist(lapply(r, sample, 1000))
ist <- lapply(strsplit(is, ' '), function(x) paste(x[1 : (length(x)-1)], collapse = " "))
isa <- sapply(strsplit(is, ' '), function(x) paste(x[length(x)], collapse = " "))
isa <- tokenize(toLower(isa), removePunct = T, removeSeparators = T)
```


```r
npr.text <- NULL
for (i in 0:11){
  url2 <- paste0("http://api.npr.org/query?id=1007,3&numResults=20&output=JSON&startNum=", (20*i)+1,
                 "&apiKey=", readLines("test/npr.key"))
  npr.json <- fromJSON(url2)
  for (j in 1: length(npr.json$list$story$text$paragraph)){
    if(nrow(npr.json$list$story$text$paragraph[[j]]) > 1){
      u <- unclass(npr.json$list$story$text$paragraph[[j]]$`$text`)
      npr.text [[220 + j]] <- u[1:(length(u) -1)]
    }
  }
}
npr.text <- unlist(npr.text)[1:3000]
npr.text <- npr.text[-correct] #filter out initial run of 30 matches which were badly formed lines
ost <- lapply(strsplit(npr.text, ' '), function(x) paste(x[1 : (length(x)-1)], collapse = " "))
osa <- sapply(strsplit(npr.text, ' '), function(x) paste(x[length(x)], collapse = " "))
osa <- tokenize(toLower(osa), removePunct = T, removeSeparators = T)
```

The first model (Model 1) tested is utilized the basic backoff approach. Here we implement no
parameters whatsoever. If the phrase is found in the highest order model (4-gram), we return that
model's answer. If it is not found, we back off to the next lowest order model (3-gram). We perform
this recursively until we get to the uni-gram model, at which point we simply return "the". Model 1
which calculates maximum likelihood and next-word predictions for each corpus, then combines them, breaking ties by whichever corpus has the higher frequency. 


```r
pred <- lapply(ist, stupidBackoff)
pred.oos <- lapply(ost, stupidBackoff)
```

**Model 1 Performance**

```r
#for(i in 1 : length(pred))  comp <- rbind(comp, c(isa[[i]], pred[[i]]))
#read from pre-computed
comp <- readRDS("test/compare_3000_stupid_backoff.RDS")
comp.oos <- readRDS("test/compare_2970_stupid_backoff.RDS")
```

Test Set     Correct Tests Percent   # "the"
--------     ------- ----- -------   -----------
In-sample    415     3000    13.8333   1133
Out-of-sample413     2970    13.9057   1129


Model 2 also utilizes the basic non-parameterized backoff approach. The difference here is how the
*n*-gram models are created. Model 2 combines all the corpora and then calculates maximum
likelihood over the entire set of *n*-grams. This resulted in 211,539 different values (of 4,110,042), but the in-sample performance was the same.



**Model 2 Performance**


```r
pred2 <- lapply(ist, stupidBackoff)
pred2oos <- lapply(ost, stupidBackoff)
```


```r
#for(i in 1 : length(pred))  comp <- rbind(comp, c(isa[[i]], pred2[[i]]))
#read from pre-computed
comp2 <- readRDS("test/compare_3000_stupid_backoff_single.RDS")
comp2oos <- readRDS("test/compare_2970_stupid_backoff_single_OOS.RDS")
```

Test Set     Correct Tests Percent   # "the"
--------     ------- ----- -------   -----------
In-sample    415     3000    13.8333   1133
Out-of-sample413     2970    13.9057   1129


We see identical performance between models 1 and 2. We will utilize model 2 going forward since it
is less computationally costly. Also note the in-sample and OOS rates are nearly identical, with the
OOS performing a little worse, as should be expected. 

