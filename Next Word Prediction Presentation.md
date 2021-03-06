Next Word Prediction 
========================================================
font-family: 'Helvetica'
Joe Willage &nbsp;&nbsp;  ([@lustyandlewd](http://twitter.com/lustyandlewd))  
Jan 2016  

Overview:


Pre-processing
========================================================
- $n$**-gram tokenization**
  - The raw text is converted to lowercase, and punctuation and whitespace is removed.
  - The output is fed into a tokenize function, which breaks the text into 
  1, 2, 3, 4, and 5-gram tokens.
  
- **Model Creation**
  - Each $n$-gram collection is fed into a create model function.
  - The function filters out unwanted grams, i.e. those containing profanity, numbers, special 
  characters, etc. Filtering on this level ensure that we don't have incomplete sentences.
  - The frequency percentage is calculated for each gram using the `table` function. The $n$-gram
  with the top $5$ frequencies for each $n-1$ index is ordered and saved. This becomes the model.

NLP Algorithms
========================================================
<style>
.reveal h1{
font-size: 30px;
}
.reveal h5{
font-size: 22px;
}
.reveal pre code {
  display: block; padding: 0.5em;
  font-size: 1.6em;
  line-height: 1.1em;
  background-color: white;
  overflow: visible;
  max-height: none;
  word-wrap: normal;
  font-size:14px;
}
</style>

- **Stupid Backoff**
  - <h5>In ordinary stupid backoff, input is fed into the highest order model (5-gram). If a prediction
  is found it is returned. If not, the next highest order model is searched. This process recurses
  until there is a match. If there is no match by the 2-gram model, the most frequent 1-gram is
  returned (typically "the").  
  - <h5>This app returns the top $k$ results, as requested by the user (up to 5). This might span multiple models if the highest order model 
  doesn't contain $k$ matches. For example, take the phrase "the san antonio" with $k$=5. This 3 word index
  is found twice in the 4-gram model. To satisfy the remaining 3 predictions, the app returns the 
  top three results from the 3-gram model. 
  
  ```
         gram    Freq n
        river 1.6e-07 4
   silverfish 1.6e-07 4
        spurs 9.1e-07 3
           tx 7.6e-07 3
          and 6.1e-07 3
  ```
  Note that the lower order results actually return a higher frequency. But backoff chooses the 
  higher order, regardless of frequency.
  - <h5>Part of Speech tagging 
  

NLP Algorithms (continued)
========================================================
- **Linear Interpolation**
  - All available $n$-gram models are utilized to pick the top $k$ most likely words by score.
  - Scores are equal to the following:
  $$ = \lambda_1 * Freq(w_i|w_{i-2}, w_{i-1}) +
  \\
  \lambda_2 * Freq(w_i|w_{i-1}) +
  \\
  \lambda_3 * Freq(w_i)
  $$
Lambda is parameterized and can be changed on the fly, but the app uses a value of $0.4$ for the
highest order model that a match is found, $0.4^2$ for the next highest order, etc. 

Using the App
========================================================
- When the app is started up, it reads in the models saved as RDS. This should take no more than
15 seconds, please be patient. When the "Loading" message disappears and you can enter input, 
the app is ready.
- After enterting a phrase in the textbox, the user can choose the max results ($k$), and the
method of prediction. 
- Results are displayed with the single most likely prediction on top of the list, in blue. The next
most likely results, up to $k - 1$ are displayed below that, in decreasing order of likelihood. 
- Below the results list, a plot is displayed, which shows on a 1-dimensional chart the scores 
(Freq for backoff, Scores for interpolation) of each result. 
- For backoff predictions, results in the plot are sized so that higher order $n$-grams are larger
(different order models are also different colors). Due to the fact that the app supports multiple
backoff predictions, we may see lower order results having a higher score (slide 3). 







