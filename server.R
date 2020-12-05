#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/



#install.packages("tm")
#install.packages("stringr")
#install.packages("shiny")

### Data Science Capstone : Course Project
### server.R file for the Shiny app
### Github repo : https://github.com/shubham859/Data-Science-Capstone
suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# Load Quadgram,Trigram & Bigram Data frame files

quadgram <- readRDS("Quadgram_df.RData")
trigram  <- readRDS("Trigram_df.RData")
bigram   <- readRDS("Bigram_df.RData")
unigram   <- readRDS("Unigram_df.RData")
mesg <- ""

# Cleaning of user input before predicting the next word
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

predicted_word <- function(x) {
  xclean <- cleaning(x)
  xs <- strsplit(xclean, " ")[[1]]
  most_common <- unigram$unigram[1]
  # Predict the next term of the user input sentence
  # 1. For prediction of the next word, Quadgram is used first
  # 2. If no Quadgram is found, then the Trigram is used
  # 3. If no Trigram is found, then the Bigram is used
  # 4. If no Bigram is found, then the word with highest frequency is returned
  
  
  if (length(xs)>= 3) {
    xs <- tail(xs,3)
    pattern <- str_flatten(xs, " ")
    match_index <- which.max(str_starts(quadgram$term, pattern))
    if (is.na(match_index)) {
      mesg <- most_common
    }
    else {
      mesg <- strsplit(quadgram$term[match_index], " ")[[1]][4] 
    }
  }
  else if (length(xs) == 2){
    xs <- tail(xs,2)
    pattern <- str_flatten(xs, " ")
    match_index <- which.max(str_starts(trigram$term, pattern))
    if (is.na(match_index)) {
      mesg <- most_common
    }
    else {
      mesg <- strsplit(trigram$term[match_index], " ")[[1]][3] 
    }
  }
  else if (length(xs) == 1){
    match_index <- which.max(str_starts(bigram$term, xs))
    if (is.na(match_index)) {
      mesg <- most_common
    }
    else {
      mesg <- strsplit(bigram$term[match_index], " ")[[1]][2] 
    }
  }
  return(mesg)
}


shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    result <- predicted_word(input$inputString)
    result
  });
  
  output$text1 <- renderText({
    input$inputString});
}
)