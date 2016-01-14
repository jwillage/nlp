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

Test Set     Correct Tests Percent   # Unigram predictions
--------     ------- ----- -------   -----------
In-sample    467     3000    15.5667   
Out-of-sample293     2972    9.8587     

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

Test Set     Correct Tests Percent   # Unigram predictions
--------     ------- ----- -------   -----------
In-sample    463     3000    15.4333   
Out-of-sample313     2972    10.5316   124


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
C1) Unlimited unigrams; > 1 bigram; > optimal B trigram; optimal A 4-gram :
10.26%  

So adding frequency cutoffs did not help at all. The next thing we'll try is implementing POS 
tagging to get us a better unigram accuracy. Improving this score may also improve C1 and models 
that fall bcak to unigrams.

Model 4 includes the POS lookup table. This consists of 32 different parts-of-speech and their most
likely next word, according to our training data. 


```r
comp.pos.is <- readRDS("test/compare_3000_stupid_backoff_single_POS.RDS")
comp.pos <- readRDS("test/compare_3000_stupid_backoff_single_OOS_POS.RDS")
```

Test Set     Correct Tests Percent   # Unigram predictions
--------     ------- ----- -------   -----------
In-sample     466     2972    15.6797   190
Out-of-sample313     2972    10.5316   124

Unfortunately we get the same perform with POS prediction. Further inspection shows the most likely
response for phrases that had to fall back to uni-gram is "says" with a value of 
55. Inspection of the test set shows
that almost all of these cases were preceded with a proper noun, ie "Kate says". However, when the
input is cleaned before going through the prediciton model, it is converted to lowercase, among 
other things. This leaves the phrase "kate" which is tagged as a noun instead of a proper noun. 
However, even if the input cleanup was not an issue, the POS model was trained on data that went 
through a similar cleaning process. So there would not be many proper nouns in that training set. 
The POS model currently predicts "they" for proper nouns. Training on raw data may result in a more
accurate model that maybe predicts "says". It's also worth pointing out that the test data is from
NPR articles which includes quotes and attributations for those quotes. In practice, this may not 
be a true representation of input. The in-sample rows have "says" only once.   

We can manually override the POS model by replacing "they" with "says" for proper nouns, etc. I 
actually wouldn't consider this "manual", but a different method that can be automated: learning 
based on actual data, ML. The original POS model failed because it was predicting the most likely 
word given a POS for the prior word, which came from the training data. In actual use, the 
prior word is not from the training data (if it was in the training data we could predict a proper 
bigram), but it is unseen, and hence the POS prediction model. Given more time, we could re-train
on unseen words. 

Model 5 uses simple interpolation. In the first test, lambda is set to 0.4 for each model *k*. The
second test set lambda to 0.05.

**Model 5 Performance**


```r
comp.int <- readRDS("test/compare_3000_interpolation.RDS")
comp.int.05 <- readRDS("test/compare_3000_interpolation_0.05.RDS")
len <- nrow(comp.int) - sum(comp.int[,1] == "<NA>")
comp.int.oos <- readRDS("test/compare_3000_interpolation_OOS.RDS")
comp.int.05.oos <- readRDS("test/compare_3000_interpolation_0.05_OOS.RDS")
len.oos <- nrow(comp.int.oos) - sum(comp.int.oos[,1] == "<NA>")
```
Test Set     Correct Tests Percent   # Unigram predictions  Lambda
--------     ------- ----- -------   -----------            -------
In-sample     341     2874    11.865   190                      0.4
Out-of-sample     182  2972    6.1238   123                     0.4
In-sample     347     2874    12.0738   190                      0.05
Out-of-sample     199  2972    6.6958   123                     0.05

We see improvement as the lambda decreases, weighting the lower order *n*-grams less. As the lambda
value decreases, the model approaches stupid backoff. 

For Model 6, we increase the number of grams for each index. Model 5 took the top 5 most frequent 
grams per index. Model 6 doubles that, taking the top 10.

**Model 6 Performance**


```r
comp.int.k10.4 <- readRDS("test/compare_3000_interpolation_k10_0.4_OOS.RDS")
comp.int.k10.05 <- readRDS("test/compare_3000_interpolation_k10_0.05_OOS.RDS")  
comp.int.k10.005 <- readRDS("test/compare_3000_interpolation_k10_.005_OOS.RDS")
```
Test Set     Correct Tests Percent   # Unigram predictions  Lambda
--------     ------- ----- -------   -----------            -------
Out-of-sample     155  2972    5.2153   124                     0.4
Out-of-sample     195  2972    6.5612   124                     0.05
Out-of-sample     248  2972    8.3445   124                     0.005

Model 7 increases the number of grams to 5. We return to stupid backoff, which has so far yielded 
the best results. We now add the 5-gram to the model.

**Model 7 Performance**


```r
comp7 <- readRDS("test/compare_3000_stupid_backoff_quint.RDS")
comp7.oos <- readRDS("test/compare_3000_stupid_backoff_quint_OOS.RDS")
```

Test Set     Correct Tests Percent   # Unigram predictions
--------     ------- ----- -------   -----------
In-sample    578       2874    20.1113   6
Out-of-sample297   2972    9.9933   0

Interestingly, we see a slight decrease in OOS accuracy from Model 2. This is likely due to biasness
in the sample data. 

Although pruning low-frequency grams did not help the model earlier, we can't help but think it will
help with what is surely overfitting. 
