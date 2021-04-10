library(dplyr)
library(tm)

model <- readRDS("data/data_model.rds")
SimpleGoodTuring<-readRDS("data/sgt_model.rds")


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