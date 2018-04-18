library(dplyr)
library(rio)
library(sentimentr)

setwd('C:/Users/nnzarr/Desktop/')
source('C:/Users/nnzarr/Desktop/Dropbox/dataSciencePractice/officeLines/officeHelpers.R')

if (T){
  raw <- import('the-office-lines.xlsx')
}

#inText <- data.frame(raw$line_text[1:5])

vectorSentiments <- function(inText){
  sents <- apply(data.frame(inText), 1, function(x) mean(sentiment(x)$sentiment))
  return(sents)
}


officeSentiments <- raw %>%
  mutate(avgSent = vectorSentiments(line_text))

export(officeSentiments, 'officeLinesSentiment.xlsx')

