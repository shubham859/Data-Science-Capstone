#load libraries
library(stringr) # for string manipulation
library(dplyr) # for DF manipulation
library(ggplot2) # for visuals
library(kableExtra) # for visuals (cleaner tables)
library(shiny)
library(shinythemes) # for sleeker Shiny background 
library(shinycssloaders) # for plot animations
library(stringi) #for string manipulation
library(stringr) #for string manipulation
library(tm) #to filter out specific words
library(LaF) #to read specified percentage of the files
library(tidytext)
library(gridExtra)
library(scales)
library(data.table) #fast file read/writes
library(kableExtra) #for pretty table outputs
library(LaF) #to read specified percentage of the files

#for reproducibility
set.seed(859)

blogs <- readLines("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("Coursera-SwiftKey/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

## Generating a random sapmle of all sources
sampleTwitter <- twitter[sample(1:length(twitter), length(twitter)/50)]
sampleNews <- news[sample(1:length(news), length(news)/50)]
sampleBlogs <- blogs[sample(1:length(blogs), length(blogs)/50)]
textSample <- c(sampleTwitter, sampleNews, sampleBlogs)

profanity_words <- read.table("profanity_words.txt", header = FALSE)
cleaning <- function(text) {
  # Remov non-ascii characters
  textSample <- iconv(text, from = "UTF-8", to = "ASCII", sub = "")
  # To lowercase
  textSample <- tolower(textSample)
  # Remove extra whitespaces
  textSample <- gsub("\\s+", " ", textSample)
  # Remove hyphens, and special characters
  textSample <- gsub("(~|!|=|@|#|\\$|%|\\^|&|\\*|\\(|\\)|\\-|_|\\+|\\?|<|>|:|\\.*\")", "", textSample)
  textSample <- removePunctuation(textSample)
  # Remove "..."
  textSample <- gsub("\\...", "", textSample)
  # Remove URLs
  textSample <- gsub("http\\S+", "", textSample)
  textSample <- gsub("www\\S+", "", textSample)
  # Remove numeric characters
  textSample <- gsub("\\d+", "", textSample)
  textSample <- removeNumbers(textSample)
  # Remove sentences with profanity words
  profanity_words <- read.table("profanity_words.txt", header = FALSE)
  textSample <- removeWords(textSample, profanity_words[, 1])
  # Remove leading or trailing whitespaces
  textSample <- trimws(textSample)
  return(textSample)
}
text <- cleaning(textSample)

clean_text <- data.frame(line = 1:length(text), text = text, stringsAsFactors = FALSE)

#function to tokenize and build the df
unigram_df_build <- function(ngram_text) {
  tokenized_all <- ngram_text %>%
    unnest_tokens(output = unigram, input = text) %>%
    #filter out end of sentence markers
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), unigram)) %>%
    count(unigram) %>%
    select(unigram, n) %>%
    rename(frequency = n) %>%
    #arrange in descending order
    arrange(desc(frequency))
  
  #convert single tokens to DF
  unigram_df <- as.data.frame(tokenized_all)
  
  return(unigram_df)
}

#determine time to create the cleaned ngram_df
system.time(unigram_df <- unigram_df_build(clean_text))

#function to build the bigram dataframe
bigram_df_build <- function(ngram_text) {
  bigram_df <- ngram_text %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    na.omit() %>%
    #filter out end of sentence markers
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
    mutate(term = paste(word1, word2, sep = " ")) %>%
    count(term) %>%
    rename(frequency = n) %>%
    #remove word1 and word2
    select(term, frequency) %>%
    #arrange in descending order
    arrange(desc(frequency))
  
  return(as.data.frame(bigram_df))
}

#build the DF of bigrams and calculate the runtime
system.time(bigram_df <- bigram_df_build(clean_text))

#trigrams
trigram_df_build <- function(ngram_text) {
  trigram_df <- ngram_text %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    na.omit() %>%
    #filter out end of sentence markers
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word3)) %>%
    mutate(term = paste(word1, word2, word3, sep = " ")) %>%
    count(term) %>%
    rename(frequency = n) %>%
    #remove word1/2/3
    select(term, frequency) %>%
    #arrange in descending order
    arrange(desc(frequency))
  
  return(as.data.frame(trigram_df))
}

#build the DF of trigrams and calculate the runtime
system.time(trigram_df <- trigram_df_build(clean_text))

#quadgrams
quadgram_df_build <- function(ngram_text) {
  quadgram_df <- ngram_text %>%
    unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
    tidyr::separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
    na.omit() %>%
    #filter out end of sentence markers
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word1)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word2)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word3)) %>%
    filter(!grepl(paste0("^", "endofsentencemarker", "$"), word4)) %>%
    mutate(term = paste(word1, word2, word3, word4, sep = " ")) %>%
    count(term) %>%
    rename(frequency = n) %>%
    #remove word1/2/3
    select(term, frequency) %>%
    #arrange in descending order
    arrange(desc(frequency))
  
  return(as.data.frame(quadgram_df))
}

#build the DF of quadgrams and calculate the runtime
system.time(quadgram_df <- quadgram_df_build(clean_text))

saveRDS(unigram_df, file = "Unigram_df.RData")
saveRDS(bigram_df, file = "Bigram_df.RData")
saveRDS(trigram_df, file = "Trigram_df.RData")
saveRDS(quadgram_df, file = "Quadgram_df.RData")