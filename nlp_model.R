#===========================1. Download Data=========================================
set.seed(03282021)

# Obtaining the data from the WEB
if(!dir.exists("data")) {
  dir.create("data")
}
if(!file.exists("data/Coursera-SwiftKey.zip")) {
  download.file(
    url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
    destfile = "data/Coursera-SwiftKey.zip")
}
# Unziping the data in the current folder
if(file.exists("data/Coursera-SwiftKey.zip") &&
   !dir.exists("data/raw/final")) {
  unzip(zipfile = "data/Coursera-SwiftKey.zip",
        exdir = "data/raw")
}
#==========================2. Sample the data=========================================================================
sampled_data <- c()

tweet<-readLines("data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", warn = FALSE)
sampled_data<-c(sampled_data, sample(tweet, as.integer(length(tweet)*0.075)))
rm("tweet")

blogs<-readLines("data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", warn = FALSE)
sampled_data<-c(sampled_data, sample(blogs, as.integer(length(blogs)*0.075)))
rm("blogs")
news<-readLines("data/Coursera-SwiftKey/final/en_US/en_US.news.txt", warn = FALSE)
sampled_data<-c(sampled_data, sample(news, as.integer(length(news)*0.075)))
rm("news")
saveRDS(object = sampled_data, file = "data/raw/data_sampled.rds")
#==============================3. Clean the data===========================================================================
library(textclean)
library(dplyr)
library(quanteda)
library(tm)
library(hunspell)

library(SnowballC)
library(RWeka)

library(slam)



sampled_data <- readRDS("data/raw/data_sampled.rds")

#===============Text Cleaning Function=================
clean_text<-function(text){
  text<-replace_html(text)
  text<-replace_internet_slang(text)
  text<-replace_ordinal(text)
  text<-replace_date(text)
  text<-replace_money(text)
  text<-replace_kern(text)
  text<-replace_hash(text)
  text<-replace_names(text)
  text<-replace_incomplete(text, ".")
  text<-replace_non_ascii(text)
  text<-replace_number(text)
  text<-gsub("[\\!-]+", "!", text)
  text<-gsub("[\\?-]+", "?", text)
  text<-gsub("(.)\\1{2,}", "\\1", text)
  text<-gsub("fuck|shit|piss off|dickhead|asshole|bitch|bastard|damn|cunt|bugger|motherfucker", "", text, perl = TRUE)
  
}
#=========Sentences Cleaning Function==================
clean_sentences<-function(text){
  #Tag all end of sentence punctuation to a string to be split later
  #sentence<-data.frame(sentence = textshape::split_sentence(text))
  sentence<-data.frame(sentence = unlist(tokenize_sentence(text)))
  
  #Remove records which are blank or single whitespace
  sentence<-subset(sentence, sentence!="")
  sentence<-subset(sentence, sentence!=" ")
  
  sentence$sentence<-gsub("(^| )b( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )c( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )d( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )e( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )f( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )g( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )h( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )z( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )j( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )k( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )l( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )m( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )n( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )o( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )p( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )q( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )r( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )s( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )t( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )u( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )v( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )w( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )x( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("(^| )y( |$)", "", sentence$sentence)
  sentence$sentence<-gsub("NA", "", sentence$sentence)
  
  #sentence<-subset(sentence, nchar(sentence)>0)
  sentence
  
}
#=============Spell Check Function=========================
spell_check<-function(sentence){
  bad<-hunspell(sentence)
  len<-length(bad[[1]])
  if(len >0){
    for(i in 1:len){
      sentence<-gsub(bad[[1]][i],
                     unlist(hunspell_suggest(bad[[1]][[i]]))[1],
                     sentence)
    }
  }
  sentence<-gsub("[[:punct:]]", "", sentence)
  sentence
}

#===========================================================

sampled_data<-clean_text(sampled_data)
sampled_data<-clean_sentences(sampled_data)
sampled_data$sentence<-sapply(sampled_data$sentence, spell_check)

sampled_data$sentence<-gsub('[0-9]+', '', sampled_data$sentence)
sampled_data$sentence<-gsub(' +',' ',sampled_data$sentence) 
sampled_data$sentence<-tolower(sampled_data$sentence)

sampled_data<-subset(sampled_data, sentence!="")
sampled_data<-subset(sampled_data, sentence!=" ")


ind<-as.integer(nrow(sampled_data)*0.75)
train<-sampled_data[1:ind,]
test<-sampled_data[(ind+1):nrow(sampled_data),]

sampled_data$len<-nchar(sampled_data$sentence)
summary(sampled_data$len)

generateNGram <- function(corpus, level = 1) {
  options(mc.cores=1)
  tokenizer <- tokens(corpus, 
                      what = "word",
                      remove_symbols = TRUE,
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_url = TRUE)
  tdm <- dfm(tokens_ngrams(tokenizer, n = level), tolower = FALSE)
  tstat_nfreq<-data.frame(textstat_frequency(tdm))
  freq<-data.frame(word = tstat_nfreq$feature, freq = tstat_nfreq$frequency)
}



TwoGram <- generateNGram(train$sentence, 2)
# Split NGram in frequencies table
TwoGram_df <- within(TwoGram, word <- data.frame(do.call('rbind', strsplit(as.character(word), "_", fixed = T))))
rownames(TwoGram_df) <- 1:nrow(TwoGram_df)
TwoGram_df$w1 <- TwoGram_df$word$X1
TwoGram_df$w2 <- TwoGram_df$word$X2
TwoGram_df <- subset(TwoGram_df, select = c(w1,w2,freq))

ThreeGram <- generateNGram(train$sentence, 3)
# Split NGram in frequencies table
ThreeGram_df <- within(ThreeGram, word <- data.frame(do.call('rbind', strsplit(as.character(word), "_", fixed = T))))
rownames(ThreeGram_df) <- 1:nrow(ThreeGram_df)
ThreeGram_df$w1 <- ThreeGram_df$word$X1
ThreeGram_df$w2 <- ThreeGram_df$word$X2
ThreeGram_df$w3 <- ThreeGram_df$word$X3
ThreeGram_df <- subset(ThreeGram_df, select = c(w1,w2,w3,freq))

FourGram <- generateNGram(train$sentence, 4)
# Split NGram in frequencies table
FourGram_df <- within(FourGram, word <- data.frame(do.call('rbind', strsplit(as.character(word), "_", fixed = T))))
rownames(FourGram_df) <- 1:nrow(FourGram_df)
FourGram_df$w1 <- FourGram_df$word$X1
FourGram_df$w2 <- FourGram_df$word$X2
FourGram_df$w3 <- FourGram_df$word$X3
FourGram_df$w4 <- FourGram_df$word$X4
FourGram_df <- subset(FourGram_df, select = c(w1,w2,w3,w4,freq))

saveRDS(object = FourGram_df, file = "data/raw/data_ngram_file.rds")
#========================================================================================================================
library(dplyr)

FourGram_df <- readRDS("data/raw/data_ngram_file.rds")


FourGram_Models <- list()

model<-list()


#===================================Two Words combined===========================
#1:bigram preds
FourGram_Models$w1 <- TwoGram_df %>%
  select(w1, w2, freq) %>%
  group_by(w1, w2) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w1) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w2, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$w1) <-c("word1", "predword", "freq", "Total_Freq", "prob")


FourGram_Models$w2w3 <- ThreeGram_df %>%
  select(w2, w3, freq) %>%
  group_by(w2, w3) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w2) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w3, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w2, w3, desc(prob)) %>%  ungroup %>%
  as.data.frame(word1 = w2, predword = w3, freq = freq, Total_Freq = Total_Freq, prob = prob)
colnames(FourGram_Models$w2w3) <-c("word1", "predword", "freq", "Total_Freq", "prob")


FourGram_Models$w1w2 <- ThreeGram_df %>%
  select(w1, w2, freq) %>%
  group_by(w1, w2) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w1) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w2, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$w1w2) <-c("word1", "predword", "freq", "Total_Freq", "prob")


FourGram_Models$Tw1w2 <-FourGram_df %>%
  select(w1, w2, freq) %>%
  group_by(w1, w2) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w1) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w2, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, desc(prob)) %>% ungroup %>%
  as.data.frame(word1 = w1, predword = w2, freq = freq, Total_Freq = Total_Freq, prob = prob)
colnames(FourGram_Models$Tw1w2) <-c("word1", "predword", "freq", "Total_Freq", "prob")

FourGram_Models$Tw2w3 <-FourGram_df %>%
  select(w2, w3, freq) %>%
  group_by(w2, w3) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w2) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w2, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w2, w3, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$Tw2w3) <-c("word1", "predword", "freq", "Total_Freq", "prob")

FourGram_Models$Tw3w4 <-FourGram_df %>%
  select(w3, w4, freq) %>%
  group_by(w3, w4) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w3) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w3, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w3, w4, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$Tw3w4) <-c("word1", "predword", "freq", "Total_Freq", "prob")

#====================Combine all Bigrams============================================================

df3<-rbind(FourGram_Models$w1,
           FourGram_Models$w1w2,
           FourGram_Models$w2w3,
           FourGram_Models$Tw1w2,
           FourGram_Models$Tw2w3,
           FourGram_Models$Tw3w4)


df4<-df3 %>%
  group_by(word1, predword) %>%
  summarise(freq = sum(freq), Total_Freq = sum(Total_Freq), prob = mean(prob)) %>%
  arrange(desc(Total_Freq, prob)) %>% ungroup

model$bi<-df4

#================================================================================


#===================================Three Words combined===========================

#2.Trigram preds
FourGram_Models$w1w2w3 <- ThreeGram_df %>%
  group_by(w1, w2, w3) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w1, w2) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w3, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, w3, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$w1w2w3) <-c("word1", "word2", "predword", "freq", "Total_Freq", "prob")

FourGram_Models$Tw2w3w4 <- FourGram_df %>%
  select(w2, w3, w4, freq) %>%
  group_by(w2, w3, w4) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w2, w3) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w4, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w2, w3, w4, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$Tw2w3w4) <-c("word1", "word2", "predword", "freq", "Total_Freq", "prob")

FourGram_Models$Tw1w2w3 <- FourGram_df %>%
  select(w1, w2, w3, freq) %>%
  group_by(w1, w2, w3) %>%
  summarise_each(funs(sum(freq))) %>%
  group_by(w1, w2) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w3, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, w3, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$Tw1w2w3) <-c("word1", "word2", "predword", "freq", "Total_Freq", "prob")

#================================================================================
tf3<-rbind(FourGram_Models$w1w2w3,
           FourGram_Models$Tw2w3w4,
           FourGram_Models$Tw1w2w3)

tf4<-tf3 %>%
  group_by(word1,word2, predword) %>%
  summarise(freq = sum(freq), Total_Freq = sum(Total_Freq), prob = mean(prob)) %>%
  arrange(desc(Total_Freq, prob)) %>% ungroup

model$ti<-tf4
#================================================================================

#Tetragram preds
FourGram_Models$w1w2w3w4 <- FourGram_df %>% 
  group_by(w1, w2, w3) %>%
  mutate(Total_Freq = sum(freq)) %>%
  group_by(w4, add = TRUE) %>%
  mutate(prob = freq / Total_Freq) %>%
  arrange(w1, w2, w3, w4, desc(prob)) %>% ungroup %>%
  as.data.frame()
colnames(FourGram_Models$w1w2w3w4) <-c("word1", "word2", "word3", "predword", "freq", "Total_Freq", "prob")

model$tti<-FourGram_Models$w1w2w3w4

saveRDS(object = FourGram_Models, file = "data/clean/data_model.rds")

#========================================================================================================================
library(dplyr)
library(tm)
library(SnowballC)
library(cld3)

data_clean_dir <- "data/clean"
data_sgt_file <- "sgt_model"
data_model_file <- "data_model"
data_predictor <- "predictor_api"

FourGram_Models <- readRDS("data/clean/data_model.rds")


calculateSimpleGoodTuring <- function(model){
  
  freqTable <- table(model$freq)
  
  SGT_DT <- data.frame(
    r=as.numeric(names(freqTable)),
    n=as.vector(freqTable),
    Z=vector("numeric",length(freqTable)),
    logr=vector("numeric",length(freqTable)),
    logZ=vector("numeric",length(freqTable)),
    r_star=vector("numeric",length(freqTable)),
    p=vector("numeric",length(freqTable)))
  
  num_r <- nrow(SGT_DT)
  
  for (j in 1:num_r) {
    if(j == 1) {
      r_i <- 0
    } else {
      r_i <- SGT_DT$r[j-1]
    }
    if(j == num_r) {
      r_k <- SGT_DT$r[j]
    } else {
      r_k <- SGT_DT$r[j+1]
    }
    SGT_DT$Z[j] <- 2 * SGT_DT$n[j] / (r_k - r_i)
  }
  
  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y <- r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else {
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      if(length(n_r_plus_1) == 0 ) {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      } else {
        n_r <- SGT_DT$n[j]
        x<-(r_plus_1) * n_r_plus_1/n_r
        if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
          SGT_DT$r_star[j] <- x
        } else {
          SGT_DT$r_star[j] <- y
          use_y = TRUE
        }
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)
}
#=============================================================================================


SimpleGoodTuring <- list()

#==================Get SGT Values===============================
SimpleGoodTuring$bi<-calculateSimpleGoodTuring(model$bi)
SimpleGoodTuring$ti<-calculateSimpleGoodTuring(model$ti)
SimpleGoodTuring$tti<-calculateSimpleGoodTuring(model$tti)

SimpleGoodTuring$w1w2w3 <- calculateSimpleGoodTuring(FourGram_Models$w1w2w3)
SimpleGoodTuring$w2w3 <- calculateSimpleGoodTuring(FourGram_Models$w2w3)
SimpleGoodTuring$w3 <- calculateSimpleGoodTuring(FourGram_Models$w3)
SimpleGoodTuring$w1w3 <- calculateSimpleGoodTuring(FourGram_Models$w1w3)
SimpleGoodTuring$w1w2 <- calculateSimpleGoodTuring(FourGram_Models$w1w2)
SimpleGoodTuring$w1 <- calculateSimpleGoodTuring(FourGram_Models$w1)
SimpleGoodTuring$w2 <- calculateSimpleGoodTuring(FourGram_Models$w2)
SimpleGoodTuring$w4 <- calculateSimpleGoodTuring(FourGram_Models$w4)

saveRDS(object = SimpleGoodTuring, file = "data/clean/sgt_model.rds")


FourGramPredict <- function(dframe, SimpleGoodTuring, finalList, ngram) {
  if(nrow(ngram) > 0 & !(nrow(dframe) > 0)) {
    ngram$Probability <- sapply(ngram$freq, FUN = function(x) SimpleGoodTuring$p[SimpleGoodTuring$r == x])
    ngram <- ngram %>% select(predword, Probability)
    if(!is.null(finalList) & nrow(ngram) > 0) {
      ngram <- ngram %>% filter(predword %in% finalList) %>% ungroup
    }
    eval.parent(substitute(dframe <- ngram))
  }
}



NextWordPredict <- function(sentence, FourGram_Model, SimpleGoodTuring, finalList=NULL) {
  
  #options("scipen"=100, "digits"=8)
  
  SentenceCount <- unlist(strsplit(sentence," "))
  WordCount <- length(SentenceCount)
  
  dframe <- data.frame(predword = factor(), Probability = numeric())
  
  FourGramPredict(dframe, SimpleGoodTuring$tti, finalList,
                  model$tti %>% filter(word1 == SentenceCount[WordCount-2],
                                                    word2 == SentenceCount[WordCount-1],
                                                    word3 == SentenceCount[WordCount]) %>%
                    ungroup)
  
  FourGramPredict(dframe, SimpleGoodTuring$ti, finalList,
                  model$ti %>% filter(word1 == SentenceCount[WordCount-1],
                                                  word2 == SentenceCount[WordCount]) %>%
                    ungroup)
  
  FourGramPredict(dframe, SimpleGoodTuring$bi, finalList,
                  model$bi %>% filter(word1 == SentenceCount[WordCount]) %>%
                    ungroup)
  
  return(dframe %>% arrange(desc(Probability)))
  
}

WordPredict <- function(sentence) {
  sentence <- stripWhitespace(sentence)
  sentence <- tolower(sentence)
  sentence <- removeNumbers(sentence)
  sentence <- removePunctuation(sentence, preserve_intra_word_dashes = TRUE)
  SentenceCount <- unlist(strsplit(sentence," "))
  WordCount <- length(SentenceCount)
  if(WordCount >= 3) {
    return(NextWordPredict(paste(
      SentenceCount[WordCount-2],
      SentenceCount[WordCount-1],
      SentenceCount[WordCount]), predictor.FourGram_Model, predictor.SimpleGoodTuring))
  } else if(WordCount == 2) {
    return(NextWordPredict(paste(
      "-",
      SentenceCount[WordCount-1],
      SentenceCount[WordCount]), predictor.FourGram_Model, predictor.SimpleGoodTuring))
  } else if(WordCount == 1) {
    return(NextWordPredict(paste(
      "-",
      "-",
      SentenceCount[WordCount]), predictor.FourGram_Model, predictor.SimpleGoodTuring))
  }
}


predictor <- list()
predictor.FourGram_Model <- model
predictor.SimpleGoodTuring <- SimpleGoodTuring
predictor.WordPredict <- WordPredict

predictor.WordPredict("where are")
#=========================Test the accuracy of the model using Test data=====================================
totalWords <- 0
rightWords <- 0
for(i in 1:length(test$sentence)) {
  sentence <- unlist(strsplit(test$sentence[i]," "))
  n <- length(sentence)
  if(n > 3) {
    for(i in 1:(n - 3)) {
      wordsPredicted <- predictor.WordPredict(sprintf("%s %s %s", sentence[i], sentence[i + 1], sentence[i + 2]))
      totalWords <- totalWords + 1
      if(sentence[i + 3] %in% head(wordsPredicted$predword)) {
        rightWords <- rightWords + 1
        
      }
    }
    print(round((rightWords/totalWords)*100, 2))
  }
}

#===========================================================================