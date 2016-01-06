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
comp.oos <- readRDS("test/compare_3000_stupid_backoff_OOS.RDS")
NAs <- sum(comp.oos[,1] == "<NA>")
len <- nrow(comp.oos) - NAs
```

Test Set     Correct Tests Percent   # "the"
--------     ------- ----- -------   -----------
In-sample    593     3000    19.7667   414
Out-of-sample293     2972    9.8587     388

This initial model gives us an in-sample accuracy rate of close to 20%. As expected, the OOS rate is 
lower.

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
comp2oos <- readRDS("test/compare_3000_stupid_backoff_single_OOS.RDS")
```

Test Set     Correct Tests Percent   # "the"
--------     ------- ----- -------   -----------
In-sample    589     3000    19.6333   429
Out-of-sample313     2972    10.5316   403


The in-sample accuracy is about the same as model 1, but the OOS accuracy jumps up from that model. 
We are also predicting more "the"s, but hopefull this will be fixed when we implement POS tagging. 
We'll be using this "combined" model 2 as the one to improve upon going forward. We'll also focus
more on the out of sample tests (of course over time those will also become in-sample, so we'll 
need a validation set at the very end to give us a good approximation of our accuracy). We'll begin
looking at how different parameteres effect the accuracy and also what order model is selected. 
Let's see how model 2 OOS did with regards to model hits:


```r
rbind(table(comp2oos[,3]) ,append(0, table(comp2oos[comp2oos[,1]==comp2oos[,2], 3])))
```

```
##        1   2    3   4
## [1,] 124 665 1225 986
## [2,]   0  47  112 154
```

Most of our hits came from the 4-gram model, with unigram "the" predicting correctly 0 times.

Model 3 builds on the combined approach in model 2 but trims back the *n*-gram models. We elminiate 
*n*-grams that are only seen rarely. This is dual purpose: first it reduces the size of our models;
second it may eliminate bias in the training data. We'll implement an approach similar to ANOVA, 
adjusting the frequency cutoff for a single  *n*-gram model at a time. If the adjustment improves the model, we adjust further. When no more accuracy is gained, we backoff to the next lower model.
Disclaimer: This is tricky since the optimal cutoffs may not appear in the order which we implement the testing. But testing every variation would require us to create models and perform $k^n$ tests, where $n$ = the
number of *n*-gram models (4) and $k$ = the number of parameters to test (cutoff values, say $k = 5 (0:4)$)


```r
comp.1000 <- readRDS("test/compare_1000_baseline.RDS")
NAs.1000 <- sum(comp.1000[,1] == "<NA>"); len.1000 <- nrow(comp.1000) - NAs.1000
comp.a1 <- readRDS("test/compare_1000_a1.RDS")
comp.a2 <- readRDS("test/compare_1000_a2.RDS")
comp.b1 <- readRDS("test/compare_1000_b1.RDS")
comp.b2 <- readRDS("test/compare_1000_b2.RDS")
comp.c1 <- readRDS("test/compare_1000_c1.RDS")
```

The following models were tested on a subset of 1000 out of sample tests:  

Baseline) No cutoffs: 10.26%  
A1) Unlimited unigrams; unlimited bigrams; unlimited trigrams; > 1 4-gram :
9.56%  
A2) Unlimited unigrams; unlimited bigrams; unlimited trigrams; > 3 4-gram :
9.15%  
B1) Unlimited unigrams; unlimited bigrams; > 1 trigram; optimal A 4-gram :
9.76%  
B2) Unlimited unigrams; unlimited bigrams; > 3 trigram; optimal A 4-gram :
9.05%  
C1) Unlimited unigrams; unlimited bigrams; > 1 trigram; optimal A 4-gram :
10.26%  

So adding frequency cutoffs did not help at all. The next thing we'll try is implementing POS 
tagging to get us a better unigram accuracy. Improving this score may also improve C1 and models 
that fall bcak to unigrams.

