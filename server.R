library(rtweet)
# # # # rt <- search_tweets("#Covid_19", n = 500, include_rts = FALSE)
tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews","msnbc"), n = 100)


library(dplyr)
library(tidyverse)
server <- function(input, output,session) {
  output$introtext1 <- renderUI({
    str1 <- h2("Data Description")
    str2 <- h4("The R package rtweet have built-in functionality to scrape data from twitter.
               As the pandemic still going on around the world, it would be very interesting
               to understand what's in people's minds about Covid 19. With that, I scrape tweets
               with hashtag 'Covid_19' to analyze the most recent tweets about Covid 19. This 
               dataset includes 18,000 most recent original tweets with associated statistics including
               text, source, length, count of favoriates and retweets, etc.")
    str3 <- h4("Another interesting direction to look at twitter data is, to look at what tradistional
               medias are talking about during this Pandemic time. This file includes most recent 3,000
               twettes from each of the media in the list: CNN, BBCWorld, FoxNews, MSNBC. Fields
               includes, similarly, text, length, whether it's a retweet, hashtags, etc.")
    HTML(paste(str1, str2,str3, sep = '<br/>'))
  })
  output$introtext2 <- renderUI({
    str1 <- h2("Analysis Description")
    str2 <- h4("The first step of analysis is to explore the dataset we have. We can look at them by
               histrogram, bar-chart, plot, box-plot etc. The main goal of the exploration is to scratch
               the surface of internal relationship between varaibles, or simply to know more about
               the data sets we have. I performed very simple exploration on the datasets I have.")
    str3 <- h4("Second step is Principle Component Analysis. This procedure can help us reduce the 
               dimension of predictors, which is super helpful when there are lots of correlated variables.
               With that, I also perform simple PCA analysis on the datasets I have")
    str4 <- h4("Third step is modeling. So regression model is a way that we tell the relationship between
               variables and response. The response could be a continous variable, binary variable, or categorical.
               The goal of regression modeling is to build a fit but not over-fit model that can explain
               the underlying relationship and/or make a good prediction.")
    str5 <- h4("Fourth step / Last Step is sharing the source data for other researchers. This is an 
                important step for knowledge share and finding verification. Here, they can
                subset the dataset based on their interest.")    
    HTML(paste(str1, str2,str3,str4,str5, sep = '<br/>'))
  })
  output$introtext3 <- renderUI({
    str1 <- h2("APP Functionality")
    str2 <- h4("The functionality of this APP was built exactly following the flow of analysis: Starting
               from exploration step, in which user can save data and plot that they are exploring. And PCA
               analysis widget that user can specify aspects of the algorithm. In the modeling section,
               user can choose the variables at their will into the model. Also, after building the model,
               users are able to select values of predictors into the model and make a prediction. The last 
               section gives users an option to scroll through the data with their desired filters.")
    HTML(paste(str1, str2,sep = '<br/>'))
  })
  output$simplestatistics <- renderUI(
    h2(HTML(paste("Simple Summary Statistics about: ",input$numvarselect)))
  )
  output$stats_cnn <- renderUI(
    h3(paste("CNN: ",HTML(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="cnn")))))
  )
  output$stats_bbc <- renderUI(
    h3(paste("BBCWorld: ",HTML(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="BBCWorld")))))
  )
  output$stats_fox <- renderUI(
    h3(paste("Foxnews: ",HTML(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="foxnews")))))
  )
  output$stats_msnbc <- renderUI(
    h3(paste("MSNBC: ",HTML(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="msnbc")))))
  )
  output$stats_hist <- renderPlot(
    tmls$screen_name <- as.factor(tmls$screen_name)
    tmls %>% ggplot()
  )
}

# c(CNN="cnn", BBC="BBCWorld", FOX="foxnews",MSNBC="msnbc",All="all")

              