#' @author Janina Pohl 
#' 
#'  Functions for calculating and displaying metrics and plots 
#'  The server uses the IDs from the UI and fills them with functions 
#'  

# Load the util-file and UI 
source("ui.R")

#' Function which connects the UI with the graphical and general functions used for calculating the metrics
#' 
#' @param input Input objects defined in the UI 
#' @param output Output objects displayed in the UI 
#' @param session The session ID of the dashboard 
#' 
#' 
server <- function(input, output, session) {
  #### Reactive part ----
  # Definition of the variables which should be updated if a user clicks on certain plots  
  
  # Function to react to the input data set the user can define in a drop-down-menue: original Tweets, Retweets or all 
  dataset <- reactive ({
    get(input$dataset) 
    })
  
  # Definition of the click on the Tweet activity plot: filters the relevant data for this point in time from the whole data set  
  selected <- reactive({
    displayed <- dataset() 
    selected_points <<- nearPoints(displayed, input$plot_click, threshold = 10)
    return(selected_points)
  })
  
  # Definition of the click on the Tweet activity plot: filters the distinct user Tweeting at this specific point in time from the whole data set  
  distinct_user <- reactive({
    displayed <- dataset() 
    d_u <<- nearPoints(displayed[!duplicated(displayed$user_id),], input$plot_click, threshold = 10)
    return(d_u)
  })
  
  # Definition of the click on the Boxplots to display detailed information about the user 
  boxinfo <- reactive({
    nearPoints(selected(), input$box_points, threshold = 10, maxpoints = 1,addDist = FALSE)
  })
  
  # Filters the distinct users for the display in the age barplots: if more than 20 users are Tweeting at a specific point in time, only the most Tweeting users are returned 
  active_user <- reactive({
    displayed <- dataset() 
    selected_points <<- nearPoints(displayed, input$plot_click, threshold = 10)
    if(nrow(selected_points) < 20){
      a_u <- selected_points %>% group_by(screen_name) %>% count() %>% arrange(desc(n)) %>% .[1:nrow(selected_points),] %>% transform(., screen_name=reorder(screen_name, -n))
    } else{
      a_u <- selected_points %>% group_by(screen_name) %>% count() %>% arrange(desc(n)) %>% .[1:20,] %>% transform(., screen_name=reorder(screen_name, -n))
    }
    return(a_u)
  })
  
  # Filters the distinct users for the display in the device barplots: if more than 20 users are Tweeting at a specific point in time, only the most Tweeting users are returned 
  apps_user <- reactive({
    displayed <- dataset() 
    selected_points <<- nearPoints(displayed, input$plot_click, threshold = 10)
    if(nrow(selected_points) < 20){
      da_u <- selected_points  %>% filter(!is.na(source)) %>% group_by(source) %>% count() %>% arrange(desc(n)) %>% .[1:nrow(selected_points),] %>% transform(., source=reorder(source, -n)) %>% filter(!is.na(source))
    } else{
      da_u <- selected_points  %>% filter(!is.na(source)) %>% group_by(source) %>% count() %>% arrange(desc(n)) %>% .[1:20,] %>% transform(., source=reorder(source, -n))
    }
    return(da_u)
  })
  
  # Execution of the sentiment anaysis for the Tweets for a specific point in time 
  senti <- reactive({
    sentences <- get_sentences(selected()$text)
    sentiment_analysis <- sentiment_by(sentences,
                                       polarity_dt = lexicon::hash_sentiment_jockers_rinker, 
                                       valence_shifters_dt = lexicon::hash_valence_shifters, 
                                       amplifier.weight = 0.8, n.before = 5, n.after = 2,
                                       question.weight = 1, adversative.weight = 0.25)
    return(sentiment_analysis)
  })
  
  # Functions to filter the Tweets for a specific point in time for certain hashtags defined by the user 
  observe({
    x <- selected() %>% filter(!is.na(hashtags)) %>% .[,"hashtags"] %>% getTermMatrix(.) %>% names(.)
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session = session,inputId = "hashtaglist",label = "Select a Hashtag",choices = x,selected = tail(x, 1))
  })
  

  #### Graphics Part ----
  
  # The Tweeting activity line plot which aggregates the number of Tweets per hour 
  output$timeplot <- renderPlot({
    ggplot(dataset(), aes(x = created_at, y = counter))+ 
      geom_line(colour = blue, size=1.5) + 
      theme_minimal() + 
      theme(text = element_text(size=16)) + 
      xlab("Time") + 
      ylab ("Number of Tweets Created per Hour") + 
      scale_x_datetime(
        breaks = seq(as.POSIXct("2020-06-08 05:00:00"),as.POSIXct("2020-06-24"), "24 hours"),
        labels = date_format("%d %B", tz = "Europe/Berlin"),expand = c(0, 0),
        limits = c(as.POSIXct("2020-06-08 00:00:00", tz = Sys.timezone()),as.POSIXct("2020-06-24 00:00:00", tz = Sys.timezone())))
  })
  
  # The boxplot displaying the Follower/Following-ratio 
  output$box_follower_ratio <- renderPlot({
    dt <- distinct_user()[-which(distinct_user()$friends_count == 0),]
    ggplot(dt, aes(x = box, y = follower_ratio)) + 
      geom_boxplot(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      ylab (element_blank()) + 
      xlab(element_blank())
  })
  
  # The boxplot displaying the average sentence length per Tweet 
  output$box_tweets_sentences <- renderPlot({
    ggplot(selected(), aes(x = box, y = sentence_length)) + 
      geom_boxplot(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      ylab (element_blank()) + 
      xlab(element_blank())
  })
  
  # The boxplot displaying the percentage of upper case letters in Tweets 
  output$box_tweets_uppercase <- renderPlot({
    ggplot(selected(), aes(x = box, y = upper_case)) + 
      geom_boxplot(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      ylab (element_blank()) + 
      xlab(element_blank())
  })
  
  # The boxplot containing the total number of submiztted Tweets in the lifetime of the account 
  output$box_tweets_user_total <- renderPlot({
    ggplot(distinct_user(), aes(x = box, y = statuses_count)) + 
      geom_boxplot(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      ylab (element_blank()) + 
      xlab(element_blank())
  })
  
  # The bar plot displaying the age of the accounts tweeting at a specific point in time 
  output$bar_age_account <- renderPlot({
    ggplot(subset(distinct_user(),!is.na(account_created_at_year)), aes(x = account_created_at_year), na.rm=TRUE) + 
      geom_bar(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      ylab ("Number of Accounts") + 
      xlab("Year")
  })
  
  # The bar plot displaying the device the user used 
  output$bar_device <- renderPlot({
    ggplot(apps_user(), aes(x=source, y = n)) + 
      geom_col(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14), axis.text.x = element_text(angle = 90)) + 
      ylab ("Number. of Users") + 
      xlab("Application")
  })
  
  # The bar plot deplaying the (at most ) 20 most Tweeting users at that specific point in time 
  output$bar_top_users <- renderPlot({
    ggplot(active_user(),aes(x=screen_name, y = n)) + 
      geom_col(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14), axis.text.x = element_text(angle = 90)) + 
      ylab ("Number of Submitted Tweets") + 
      xlab("User Name")
  })
  
  # The bar plot displaying the age of accounts who posted specific hashtags 
  output$bar_hashtags <- renderPlot({
    du <- selected()[grepl(tolower(input$hashtaglist), tolower(selected()$hashtags)),]
    ggplot(du,aes(x=account_created_at_year)) + 
      geom_bar(fill = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14), axis.text.x = element_text(angle = 90)) + 
      ylab ("Number of Submitted Tweets") + 
      xlab("Year")
  })
  
  # The word cloud displaying the most frequently used hashtags: the larger, the more often was the hashtag used 
  output$wordcloud_hastags <- renderPlot({
    par(mar = c(0,0,0,0), oma = c(0,0,0,0))
    wordcloud(words = selected() %>% filter(!is.na(hashtags)) %>% .[,"hashtags"] %>% getTermMatrix(.) %>% names(.),
              freq = selected() %>% filter(!is.na(hashtags)) %>% .[,"hashtags"] %>% getTermMatrix(.), 
              min.freq = 1, max.words=300, random.order=FALSE, rot.per=0.35, colors=c(black, grey, ocher, lightgreen, green, green2, lightblue, blue))
  })
  
  # The plot displaying the usage frequencies of specific hashtag in the data set over time 
  output$time_hashtags <- renderPlot({
    end = max(selected()$time)
    start = min(dataset()$time) 
    relevant_df <- dataset()[dataset()$time >= start & dataset()$time <= end, ] %>% filter(!is.na(hashtags))
    relevant_df <- relevant_df[grepl(input$hashtaglist, relevant_df$hashtags),] %>% group_by(time) %>% count()
    ggplot(relevant_df, aes(x = time, y = n))+ 
      geom_line(colour = blue) + 
      theme_minimal() + 
      theme(text = element_text(size=14)) + 
      xlab("Time") + 
      ylab ("Number of Tweets Created per Hour Which Containted the Hashtag") + 
      scale_x_datetime(labels = date_format("%d %B", tz = "Europe/Berlin"),expand = c(0, 0)) + 
      expand_limits(y = 0)
  })
  
  # The density plot displaying the result of the sentiment analysis 
  output$line_senti <- renderPlot({
    ggplot()+ 
      geom_density(aes(senti()$ave_sentiment), colour=blue, size=2) + 
      xlab("Sentiment Level") + 
      ylab("Density") +      
      theme_minimal() + 
      theme(text = element_text(size=14)) 
  })
  
  #### Metrics Part ---- 
  
  # The specific hour the user clickes on in the Tweeting activity plot 
  output$Time <- renderText({paste(format(selected()$time[1], format="%A %d, "), paste(format(selected()$time[1]-(1*60*60), format="%H:00-"), format(selected()$time[1], format="%H:00")))})
  # The number of Tweets submitted in this specific hour 
  output$Nb_Tweets <- renderText({paste("Number of Tweets: ", length(unique(selected()$status_id)))})
  # The number of users tweeting at this specific hour 
  output$Nb_Users <- renderText({paste("Number of Users: ", nrow(distinct_user()))})
  # The percentage of verified accounts who tweeted in this specific hour 
  output$Perc_Verified <- renderText({paste("Verified User: ", paste(round(100/nrow(distinct_user())*length(distinct_user()$verified[which(distinct_user()$verified == TRUE)]), 2)," %"))})
  
  # The user name of the account clicked on in the boxplot 
  output$Box_UserID <- renderText({paste("User Name: ", boxinfo()$screen_name)})
  # The number of followers and following accounts the account has on which the user clicked on in the boxplot 
  output$Box_Followers <- renderText({paste("Followers: ", paste(boxinfo()$followers_count, paste("/ Following: ", boxinfo()$friends_count)))})
  # The age of the account clicked on in the boxplot 
  output$Box_Account_Age <- renderText({paste("Account Created: ", boxinfo()$account_created_at_year)})
  # The text of the Tweet the account clicked on in the boxplot issued 
  output$Box_Tweet_text <- renderText({boxinfo()$text})
  
  # The frequency a specific hashtag was used at that specific point in time 
  output$No_hashtags <- renderText({
    d <- selected() %>% filter(!is.na(hashtags)) %>% .[,"hashtags"] %>% getTermMatrix(.)
    paste("Times Used: ", d[which(tolower(names(d)) == tolower(input$hashtaglist))]) 
    })
  
  # The number of accounts using that hashtag at that specific point in time 
  output$No_users <- renderText({
    du <- selected()[grepl(tolower(input$hashtaglist), tolower(selected()$hashtags)),]
    paste("Distinct User: ", length(unique(du$user_id))) 
  })
  
  #### Table Part ---- 
  
  # The table containing the oxford criterion for each day (at most 10 days are displayed)
  output$tab_oxford <- renderTable({
    end = max(selected()$time)
    start = min(dataset()$time) 
    relevant_df <- dataset()[dataset()$time >= start & dataset()$time <= end, ]
    tab = oxford_criterion(relevant_df)
    if(nrow(tab) > 10) {
      tail(tab, 10)
    }
    else{
      tab
    }
  })
  
  # The table containing the number of positive and negative Tweets identified by the sentiment analysis 
  output$tab_senti <- renderTable({
    tab <- table(convertToBinaryResponse(senti()$ave_sentiment)) 
    df <- data.frame(
      Level = c("Positive", "Negative"), 
      Frequency = c(tab[[2]], tab[[1]])
    )
  })
  
  # The most positive Tweets identified by the sentiment analysis 
  output$tab_senti_pos <- renderTable({
    pos_tweets <- selected()[order(-senti()$ave_sentiment),names(selected()) %in% c("text", "screen_name")]
    pos_tweets <- pos_tweets[1:3,]
    colnames(pos_tweets) <- c("Tweet", "User")
    pos_tweets
  })
  
  # The most negative Tweets identified by the sentiment analysis 
  output$tab_senti_neg <- renderTable({
    neg_tweets <- selected()[order(senti()$ave_sentiment),names(selected()) %in% c("text", "screen_name")]
    neg_tweets <- neg_tweets[1:3,]
    colnames(neg_tweets) <- c("Tweet", "User")
    neg_tweets
  })
  
  # Detailed view on the Tweets and the user: table can be searched for specific key words 
  output$time_output <- DT::renderDataTable({
    DT::datatable(selected()[,c("created_at", "text", "is_retweet", "screen_name")], colnames = c("Time", "Tweet", "Retweet", "User"),
                  options = list(autoWidth = TRUE,columnDefs = list(list(width = '120px', targets = 1), list(width = '1330px', targets = 2), list(width = '60px', targets = c(3)), list(width = '90px', targets = c(4))))) %>%
      formatDate(1, method = 'toLocaleDateString',params = list('gb-Gb',  list(month = 'long', day = 'numeric', hour = "numeric", minute = "numeric"))
      )
  })
}

# Command to run the dashboard, with the UI as graphical framework and the server as functional background 
shinyApp(ui = ui, server = server) 

# Ignore warnings coming up when running the dashboard since they are only related to the dashboard being empty before a user clicked on the Tweet activity plot once 

