# Milestone



##Corpora Summary  


Before breaking down any text into *n*-grams or doing any natural language processing, we'll 
explore the texts just as they are provided to us. We have three corpora, namely: twitter; news; and
blogs. Here we see the number of texts in each corpus, along with the mean, median, and max 
characters per text.  


```
          Texts Average Median Maximum
twitter 2360148      69     64     140
news    1010242     201    185   11384
blogs    899288     230    156   40833
```

The average length of news and blogs are a bit higher than the medians. This skew is caused by some 
very long texts. The longest text in our corpora ia a blog, with more than 40 thousand characters.  



##Model Design  
To save space, we will not store all possible *n*-grams indexed by their *n-1*-grams. Instead, we 
only need to store the *n*-gram with the highest frequency of it's *n-1*-gram index. For instance, 
if we have a term frequency list as follows:  

Term    Frequency    index
------- ---------- --------
the box  .05        the  
the cat  .15        the  
the oven  .02       the  
the king  .01       the  

We will only store the highest frequency term for the given index, "the". In fact, we don't even
need the frequency at that point.  

Term     index
-------  --------
the box  the  



##Future Plans  
Plan to add a part of speech (POS) tagger for unseen n-grams. We'll create a small frequency list 
for each of the parts of speech (possibly trained on our data). Then instead of blindly returning 
the highest occuring token, we will return the highest occuring token given the POS.  

We will keep a small number of lines from each corpus as a test set. We can create lots of *n*-gram
samples from just a few lines to test our model.  

Don't use backoff only for grams that don't appear. IE Where is the n-1 gram more frequent? From 
that take the nth word for the model.  
