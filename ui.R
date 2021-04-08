
#' @author Janina Pohl 
#' 
#'  Framework for the visual part of the Dashboard. 
#'  The single parts of the dashboard are arranges here and linked to titles. 
#'  The IDs of the single elements, e.g., "Time" for the line graph displaying the Tweeting activity over time 
#'  can be referenced and filled with functionality in the server 
#'  

# This file uses the util-file to access all the functionalities and libraries defined there 
source("util.R")

ui = fluidPage(
  # Title of the dashboard 
  titlePanel(h1("Detection of Manipulation on Twitter", align = "center")),
  # Input selection: original Tweets, Retweets or all of them 
  selectInput("dataset", "Dataset:",
              c("Original Tweets" = "origintweets",
                "Retweets" = "retweets",
                "All Tweets" = "corona_app_short")),
  # The Tweeting activity with basic metadata 
  fluidRow(
    titlePanel(h2("Tweeting Activity", align = "center")),
    column(width = 2,
           titlePanel(h4("Details", align = "center")),
           wellPanel(textOutput("Time")),
           wellPanel(textOutput("Nb_Tweets")),
           wellPanel(textOutput("Nb_Users")),
           wellPanel(textOutput("Perc_Verified"))
    ), 
    column(width = 10,
           plotOutput("timeplot", click = "plot_click")
    )
  ),
  # The bar plots displaying the oxford criterion and user information 
  fluidRow(
    titlePanel(h2("User Information", align = "center")),
    column(width = 2,
           titlePanel(h4("Oxford Criterion", align = "center")),
           tableOutput("tab_oxford")
    ),
    column(width = 10,
           column(width = 4,
                  titlePanel(h4("Age of Accounts", align = "center")),
                  plotOutput("bar_age_account")
           ),
           column(width = 4,
                  titlePanel(h4("Application and Device Used for Tweeting", align = "center")),
                  plotOutput("bar_device")
           ),
           column(width = 4,
                  titlePanel(h4("Top 20 Tweeting Users", align = "center")),
                  plotOutput("bar_top_users")
           )
    )
  ),
  # The boxplots displaying account and sentence information 
  fluidRow(
    column(width = 2,
           titlePanel(h4("User Details", align = "center")),
           wellPanel(textOutput("Box_UserID")),
           wellPanel(textOutput("Box_Followers")),
           wellPanel(textOutput("Box_Account_Age")), 
           wellPanel(textOutput("Box_Tweet_text"))
    ), 
    column(width = 10,
           column(width = 3,
                  titlePanel(h4("Following/Follower-Ratio", align = "center")),
                  plotOutput("box_follower_ratio", click = "box_points")
           ),
           column(width = 3,
                  titlePanel(h4("Submitted Tweets per User (Total)", align = "center")),
                  plotOutput("box_tweets_user_total", click = "box_points")
           ),
           column(width = 3,
                  titlePanel(h4("Sentence Length per Tweet", align = "center")),
                  plotOutput("box_tweets_sentences", click = "box_points")
           ),
           column(width = 3,
                  titlePanel(h4("Percentage of Uppercase Letters per Tweet", align = "center")),
                  plotOutput("box_tweets_uppercase", click = "box_points")
           )
    ),
  ),
  # Analysis of the hashtags
  fluidRow(
    titlePanel(h2("Hashtag Information", align = "center")),
    column(width = 2,
           titlePanel(h4("Details", align = "center")),
           selectInput(inputId = "hashtaglist",label = "Hashtag:", choices = complete_hastaglist, selected = "corona"),
           wellPanel(textOutput("No_hashtags")),
           wellPanel(textOutput("No_users"))
           ),
    column(width = 10,
           column(width = 3,
                  titlePanel(h4("Most Used Hastags next to #Coronawarnapp", align = "center")),
                  plotOutput("wordcloud_hastags")
           ),
           column(width = 4,
                  titlePanel(h4("Age of Accounts for Selected Hashtag", align = "center")),
                  plotOutput("bar_hashtags")
           ),
           column(width = 5,
                  titlePanel(h4("Popularity of Selected Hastags", align = "center")),
                  plotOutput("time_hashtags")
           )
    )
  ),
  # Sentiment analysis with most polarizing Tweets 
  fluidRow(
    titlePanel(h2("Sentiment Information", align = "center")),
    column(width = 2,
           titlePanel(h4("Details", align = "center")),
           tableOutput("tab_senti")
    ),
    column(width = 10,
           column(width = 6,
                  titlePanel(h4("Overall Sentiment", align = "center")),
                  plotOutput("line_senti")
           ),
           column(width = 6,
                  titlePanel(h4("Most Polarizing Tweets", align = "center")),
                  fluidRow(
                    titlePanel(h5("Most Positive Tweets", align = "center")),
                    tableOutput("tab_senti_pos"),
                    titlePanel(h5("Most Negative Tweets", align = "center")),
                    tableOutput("tab_senti_neg")
                  )
           )
    )
  ),
  # Table for referencing single Tweets 
  fluidRow(
    titlePanel(h2("Detailed View", align = "center")),
    column(1),
    column(10, DT::dataTableOutput("time_output", width = "1600px"), 
    column(1)
    )
  )
)
