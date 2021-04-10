Smart Keyboard Pitch
========================================================
author: Bharathwaj Sundaresan
date: April 3rd, 2021
autosize: true


<font size="7"> What is it?</font>

People spend most of their time on a range of activities that involve typing. From email, text messaging, social networking etc we type content frequently.
- This product is an effort to replicate the "smart keyboard" in our smartphones which can predict the next word based on the words typed.
- This application is powered by a prediction algorithm that can make suggestions for the next word of a phrase.

![plot of chunk unnamed-chunk-1](./image.jfif)

How it works?
========================================================
The Machine Learning model is trained based on the text corpus data from different sources like **Twitter, News and Blogs**.

- Each phrase within the corpus is split based on the word and the frequencies of different occurrences are noted. For example,
the sentence <br>
*"Hey how are you. how are things with you. when are you going to come home."*  
is converted to

```
         word freq
1     how_are    2
2     are_you    2
3     Hey_how    1
4     you_how    1
5  are_things    1
6 things_with    1
```

- As seen with the Bigrams above, similar frequencies are collected for Trigrams and Tetragrams.
- Using the above data, the prediction algorithm is derived considering the [Markov Assumption](https://brilliant.org/wiki/markov-chains/#:~:text=A%20Markov%20chain%20is%20a,possible%20future%20states%20are%20fixed)
- The probabilities for next word are calculated from the above dictionary and the values are adjusted by smoothing using [Good-Turing frequency estimation.](https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation)

Data source & Model Performance
========================================================
The data for this prediction model was sourced from **Twitter, News and Blogs**. Based on the system configurations, 7.5% of data was subset from each source and combined to create an ensemble of text corpus consisting of around 500K lines of phrases.
- The data was cleaned, processed and split to single sentences. Cleaning includes spell check, expanding contraction, ignoring punctuation marks etc.
- The data is split to train/test with 75% of data used to train the model. 
- The model is evaluated on the test data (123k lines). As the model is built on n-grams, the test phrases are split to 1st three words and the same is the input to the model. The model is then evaluated based on the number of correct predictions of the fourth word for each sentence. 
- The model achieved an overall accuracy of 33% with 36% accuracy to predict News and Blogs and around 29% accuracy to predict tweets.
- Although the over accuracy is low, based on the amount of RAM this model consumes (less than 300 MB), this is a fairly efficient model to offer predictions on general and commonly used words.

Walkthrough
========================================================
The application is fairly simple to use. Just start typing on the text box section to receive recommendations for next word.

![plot of chunk unnamed-chunk-3](./application_snip.png)

- The `documentation` section in the application provides additional information on how to use the product and also provide instructions with necessary links to deploy this model.

Product Links
========================================================

- [Application]()
- [GitHub Repository]()
- [Exploratory data Analysis]()


Thank you for checking out this product.
