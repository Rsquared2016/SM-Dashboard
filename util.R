#' @author Janina Pohl 
#' 
#'  Libraries and functions used by different functions in the dashboard
#'  To save space and to not overload files, there functions were defined here 
#'  


# To import Tweets in which emoticons and German Umlaute are used, the encoding has to be defined 
Sys.setlocale("LC_CTYPE", "de_DE.UTF-8") 

#### Libraries ---- 

# Load all relevant external libraries 
library(tidyverse)
library(shiny)
library(shinyTime)
library(gridExtra)
library(NLP)
library(tm)
library(sentimentr)
library(SentimentAnalysis)
library(scales)
library(lubridate)
library(wordcloud)
library(memoise)
library(progress)
library(DT)

#### end ---- 

#### Data Sets ---- 

# Load all relevant data sets: Original Tweets, Retweets as well as all Tweets combined in one data set 
load("original_tweets.rda")
load("retweets.rda")
load("corona_warning_app-Tweets.rda")

#### end ---- 


#### Colours ---- 
# Definition of hexadecimal colour codes used in the dashboard 

black <- c("#3e3e3b")
lightgreen <- c("#7ab516")
green <- c("#008e96")
lightblue <- c("#009dd1")
blue <- c("#006e89")
ocher <- c("#b5bb56")
grey <- c("#929292")
green2 <- c("#5bbb56")


#### end ---- 

#### Functions ---- 

#' Disaggregate Tweets in a vector representation to use it for the sentiment analysis and the hashtag frequency analysis 
#' 
#' @param texts Vector with the pure Tweet texts 
#' @examples 
#' vector_rep <- getTermMatrix(origintweets$texts[1:100])
#' 
getTermMatrix <- memoise(function(texts) {
  # Suppress Warnings 
  storeWarn<- getOption("warn")
  options(warn = -1) 
  # Transform texts into a corpus object which can be modified by the functions of "NLP" and "tm" packages 
  myCorpus = Corpus(VectorSource(texts))
  # Remove any punctuation 
  myCorpus = tm_map(myCorpus, removePunctuation)
  # Remove stopwords, i.e. the words which should not be counted in the frequency analysis 
  ## (That the hashtag #CoronaWarnApp was used in nearly every Tweet is rather obvious, since this is the hashtag the Tweets were streamed for)
  myCorpus = tm_map(myCorpus, removeWords,c(stopwords("SMART"), "coronawarnapp", "CoronaWarnApp"))
  # Transform to lower texts to avoid duplicates in upper and lower letters 
  myCorpus = tm_map(myCorpus, tolower)
  # Create a document term matrix in which the frequencie sare counted 
  myDTM = TermDocumentMatrix(myCorpus,control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  # Sort according to the most used words 
  sort(rowSums(m), decreasing = TRUE)
})

# Create a vector containing all hashtags used in the data set (used for the drop-down-many in the hashtag analysis part of the dashboard)
complete_hastaglist <- corona_app_short %>% filter(!is.na(hashtags)) %>% .[,"hashtags"] %>% getTermMatrix(.) %>% names(.)

#' Function for the calculation of the oxford criterion (identify users with more than 50 Tweets / Day)
#' 
#' @param df Data Frame containing at least the users and the Tweets 
#' @return Data Frame containing the specific day for which the oxford criterion is calculated, 
#'         the maximal number of Tweets posted by a user and - 
#'         if one user issued more than 50 Tweets / Day, the user name 
#' @examples
#' oc <- oxford_criterion(origintweets)
#' 

oxford_criterion <- function(df){
  # Reformat the datetime column into just a date 
  df$day <- as.Date(x=df$time, format="Y-m-d")
  # Extract the unique days 
  days <- unique(df$day)
  d = t = id = c()
  for(i in 1:length(days)){
    # For each day, count the distinct users Tweeting on that day 
    current_day <- df[which(df$day == days[i]),]
    distinct_user <- unique(current_day$user_id)
    r = c()
    # For each of these users, check whether they tweeted more than 50 times 
    for(k in 1:length(distinct_user)){
      tweets_per_user <- current_day[which(current_day$user_id == distinct_user[k]),]
      r = c(r, nrow(tweets_per_user)) 
      # If that is the case, store the day, the user ID as well as the number of Tweets posted 
      if(r[k] >= 50){
        d = c(d, format(current_day$day[i],"%b %d"))
        t = c(t, nrow(tweets_per_user))
        id = c(id, df$screen_name[which(df$user_id == distinct_user[k])]) 
      }
    }
    # If for this day none tweeted over 50 times, store the user who Tweeted most instead 
    if(any(format(current_day$day[i],"%b %d") == d) == FALSE){
      d = c(d, format(current_day$day[i],"%b %d"))
      t = c(t, max(r))
      id = c(id, "None")
    }
  }
  # Store the result (one entry at least per day) in a data frame and return it 
  result = data.frame(
    Day = d, 
    No.Tweets = t, 
    Name = id
  )
  result <- result[order(result$Day),]
  return(result)
}

#### end ---- 
