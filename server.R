library(rtweet)
tmls <- get_timelines(c("cnn", "BBCWorld", "cnbc","msnbc"), n = 100)
library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
# library(tidyverse)
library(plotly)
library(randomForest)
library(mathjaxr)
# calcualte timing of tweet
tmls["timing_hour"] <- substr(tmls$created_at,12,13)
tmls["timing_min"] <- substr(tmls$created_at,15,16)
tmls["timing"] <- apply(tmls,1,function(data){60*as.numeric(data$timing_hour)+as.numeric(data$timing_min)})
model2_choice <- c("select all","timing", "display_text_width","retweet_count","favorite_count")
tmls["Media_CNN"] <- ifelse(tmls$screen_name=="CNN",1,0)
# server file
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
  output$simplestatistics <- renderUI({
    str0 <- h3(paste("Simple Summary Statistics about: ",input$numvarselect))
    str1_1 <- h4("CNN")
    str1_2 <- h4(toString(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="CNN"))))
    str2_1 <- h4("BBC World")
    str2_2 <- h4(toString(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="BBCWorld"))))
    str3_1 <- h4("CNBC")
    str3_2 <- h4(toString(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="CNBC"))))
    str4_1 <- h4("MSNBC")
    str4_2 <- h4(toString(summary(tmls %>% select(input$numvarselect) %>% subset(screen_name="MSNBC"))))
    HTML(paste(str0, str1_1, str1_2, str2_1, str2_2, str3_1, str3_2, str4_1, str4_2, sep = '<br/>'))
   })
  output$stats_hist <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$numvarselect]]))
    plot + geom_histogram() +
      xlab(input$numvarselect)+
      facet_wrap(~screen_name)
   })
  output$binary_grap <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$binvarselect]]))
    plot + geom_bar( fill="steelblue") +
      xlab(input$binvarselect)+ 
      geom_text(aes(label=..count..,y=..count..+2),stat="count") + 
      facet_wrap(~screen_name)

  })
  output$source_grap <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$charvarselect]]))
    plot + geom_bar(fill="steelblue") +
      xlab(input$charvarselect)+ 
      geom_text(aes(label=..count..,y=..count..+2),stat="count") + 
      facet_wrap(~screen_name)
    
  })
  output$text_table <- renderTable({
    content <- tmls %>% select(screen_name,text)
    
    cnn_text <-   content %>% subset(screen_name=="CNN") %>% slice_head(n=20)
    bbc_text <-   content %>% subset(screen_name=="BBCWorld") %>% slice_head(n=20)
    cnbc_text <-   content %>% subset(screen_name=="CNBC") %>% slice_head(n=20)
    msnbc_text <-   content %>% subset(screen_name=="MSNBC") %>% slice_head(n=20)
    
    final2 <- rbind(cnn_text,bbc_text,cnbc_text,msnbc_text)
    final2$text <- paste(substr(final2$text,1,200),"...")
    final2 
  })
  
  output$downloadData <- downloadHandler(
    filename = "TweetData.csv",
    content = function(file) {
      write.csv(final2, file)
    }
  )
  output$plotly_plot <- renderPlotly({
    fig <- plot_ly(tmls,
            x = ~.data[[input$x_axis_select]],
            y = ~.data[[input$y_axis_select]], 
            type = 'scatter',
            mode = 'markers')
    fig <- fig %>% layout(
        xaxis = list(title = "X-Axis"),
        yaxis = list(title = "Y-Axis")
      )
    fig
    
  })
  
  output$subset_table <- renderTable({
    subset_result <- tmls %>%filter(source==input$source) %>% filter(screen_name==input$media) %>%
      filter(is_retweet==input$isretw)%>% 
      filter(favorite_count>= input$favcount)
    subset_result <- subset_result[,c(4:7,12:14)]
    subset_result
  })
  
  output$download_media_data <- downloadHandler(
    file = "Subset Media Tweet.csv",
    content = function(file) {
      write.csv(subset_result, file)
    }
  )
  output$cluster_graph <- renderPlot({
    data_clustering <-  tmls %>% 
      group_by(screen_name) %>%
      sample_n(input$obs)
    
      hierClust <- hclust(dist(data.frame(data_clustering$timing, data_clustering$display_text_width, data_clustering$retweet_count,data_clustering$favorite_count)),
                          method=input$agglo)
      plotresult <- plot(hierClust,xlab="")
      plotresult
  })
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("Cluster Download.png") },
    content = function(file) {
      ggsave(file, plot= plotresult, device = "png")
    }
  )
  observe({
  treedata <-  tmls %>% 
    group_by(screen_name) %>%
    sample_n(input$treeobs) %>% 
    select(screen_name, timing, display_text_width,retweet_count,favorite_count)
    rfTree <- randomForest(as.factor(screen_name) ~ ., data = treedata, mtry = input$vartry, ntree=input$treecount)
  })
  
  output$randomforest <- renderPlot({
    plot(rfTree, type="l",main = "Error Rate by Tree Count")
    legend("topright", legend=unique(treedata$screen_name), col=1:4, pch=19)
  })
  output$treepred <- renderUI({
    newdata <- data.frame(timing=input$treepred_timing, display_text_width=input$treepred_length,
                          retweet_count=input$treepred_retweetcount,favorite_count=input$treepred_favocount)
    pre_rfTree <- predict(rfTree, newdata)
    as.character(pre_rfTree)
  })
  observe({
    if("select all" %in% input$regression_var)
      selected_choices=model2_choice[-1]
    else
      selected_choices=input$regression_var
    updateSelectInput(session,"regression_var",selected = selected_choices)  
  })
  output$selected <- renderText({
    paste(input$regression_var, collapse = ",")
  })
  # formular of the function
  output$ex3 <- renderUI({
    withMathJax(
      helpText('The Link function is Logit:
               $$logit(Y=1|X) = {\\beta_0 + \\beta_1Predictor1 + \\beta_2Predictor2 + \\beta_3Predictor3...}\\!$$'))
  })
  # regression core
  output$regression <- renderText({
    if("select all" %in% input$regression_var)
      selected_choices=model2_choice[-1]
    else
      selected_choices=input$regression_var
    formular <- as.formula(Media_CNN~[[i]])
    LR_predictors <- paste(noquote(paste(selected_choices,collapse = "+"))
    
    # fit <- glm(Media_CNN~timing+display_text_width+retweet_count+favorite_count,data=tmls,family=binomial())
    # summary(fit) # display results
    # confint(fit) # 95% CI for the coefficients
    # exp(coef(fit)) # exponentiated coefficients
    # exp(confint(fit)) # 95% CI for exponentiated coefficients
    # predict(fit, type="response") # predicted values
    # residuals(fit, type="deviance") # residuals
    # 
    # table(predict(fit, type="response") > 0.2,tmls$Media_CNN)
    # 
    # predict(fit, newdata, type="response")
  })
}
