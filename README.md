### ST 558 Project 3 ###
### Author: Xinyu Hu ###
### Date 7/27/2020   ###
########################

# Load library

library(rtweet)
library(ggplot2)
library(shiny)
library(dplyr)
library(stringr)
library(plotly)
library(randomForest)
library(mathjaxr)

# Load library
library(shinydashboard)


# create UI
ui <- dashboardPage(
  # define default choice for modeling
  skin="red",
  # assign dashboad header
  dashboardHeader(title = "Media Tweet Analysis"),
  
  # Set dashboard side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("table")),
      menuItem("Numeric Exploration", tabName = "explore_num", icon = icon("table")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("bar-chart-o")),
      menuItem("Random Forest", tabName = "model1", icon = icon("bar-chart-o")),
      menuItem("Logsitic Regression", tabName = "model2", icon = icon("bar-chart-o")),
      menuItem("More On Data", tabName = "data", icon = icon("list-alt"))
    )
  ),
  
  # Set the body of dashboard
  dashboardBody(
    tabItems(
      # The introduction page
      tabItem(tabName = "intro", 
              div(HTML("<em><h2> Introduction</em>")),
              fluidRow(
                column(12,
                box(htmlOutput("introtext1"),background = "maroon",
                    tags$a(href="https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html", 
                       h4("More Details on the rtweet package")))
                ),
                column(12,
                box(htmlOutput("introtext2"),background = "orange",
                    tags$a(href="https://xhu23.github.io/about/", h4("Contact me if you have any question")))
                ),
                column(12,
                box(htmlOutput("introtext3"),background = "blue",
                    tags$a(href="https://xhu23.github.io/about/", h4("Contact me if you have any question")))
                )
              )
      ),
      # general exploration page
      tabItem(tabName = "explore",
              div(HTML("<em><h2> Data Exploration</em>")),
              fluidRow(
                box(
                  # select of variable type will change the available selection below
                  selectInput("vartype", "Please Select Variable Type of Interest",
                              c(Numeric = "num", Binary = "bin",Character="char")),
                  conditionalPanel(
                    condition = "input.vartype == 'num'",
                    selectInput("numvarselect", h3("Select NUMERIC Variable"),
                              c("display_text_width","favorite_count","retweet_count"))
                  ),
                  conditionalPanel(
                    condition = "input.vartype == 'bin'",
                    selectInput("binvarselect", h3("Select BINARY Variable"),
                              c("is_retweet","is_quote"))
                  ),
                  conditionalPanel(
                    condition = "input.vartype == 'char'",
                    selectInput("charvarselect", h3("Select CHARACTER Variable"),
                              c("text","source"))
                  )
                ),
                box(
                  conditionalPanel(
                    condition = "input.vartype == 'num'",
                    uiOutput("simplestatistics"),
                    plotOutput("stats_hist")
                  ),
                  conditionalPanel(
                    condition = "input.vartype == 'bin'",
                    plotOutput("binary_grap")
                  ),
                  conditionalPanel(
                    condition = "input.vartype == 'char'",
                    conditionalPanel(
                      condition = "input.charvarselect == 'source'",
                      plotOutput("source_grap")
                    ),
                    conditionalPanel(
                      condition = "input.charvarselect == 'text'",
                      tags$text(h3("Most Recent 20 Tweeter Text (Each Media)")),
                      downloadLink("downloadData", h2("Download Table")),
                      tableOutput("text_table")
                    )
                  )
                )
                )
              
      ),
      # The numeric exploration page
      tabItem(tabName = "explore_num",
              div(HTML("<em><h2>Numeric Exploration</em>")),
              box(
                tags$text(h3("Explore Relationship Between Numeric Varaibles")),
                selectInput("x_axis_select", h3("Select X-Axis Variable"),c("display_text_width","favorite_count","retweet_count")),
                selectInput("y_axis_select", h3("Select Y-Axis Variable"),c("display_text_width","favorite_count","retweet_count")),
                plotlyOutput("plotly_plot")
              )
      ), 
      # The cluster analysis page
      tabItem(tabName = "cluster",
              div(HTML("<em><h2> Cluster Analysis</em>")),
              box(
                selectInput("agglo", h3("Select Agglomeration Method"),c("ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid")),
                numericInput("obs", h3("Number of Tweets (each Media) for Clustering"), min = 1,max = 100, value = 20),
                downloadButton('downloadPlot',"Download Plot"),
                plotOutput("cluster_graph")
              )
        
      ),
      # Random Forest Page
      tabItem(tabName = "model1",
              div(HTML("<em><h2>Random Forest Model</em>")),
              fluidRow(
                       box(
                         tags$text(h3("Random Forest Model")),
                         tags$text(h4("Response: Media")),
                         tags$text(h4("Predictor: Tweet Timing, Text Width, Retweet Count, Favorite_Count")),
                         numericInput("treecount", h4("Select Number of Trees to Grow"), min = 1,max = 200, value = 5),
                         numericInput("vartry", h4("Select Number of Variables randomly tried at each node"), min = 1,max = 4, value = 1),
                         numericInput("treeobs", h4("Number of Tweets (each Media) for Random Forest"), min = 1,max = 200, value = 10),
                         verbatimTextOutput("randomforest_summary"),
                         plotOutput("randomforest")
                       ),
                       box(
                         tags$text(h3("Dynamic Prediction")),
                         numericInput("treepred_timing",h4("Input: Timing of the day (minutes)"), min=1, max=60*24, value=480),
                         numericInput("treepred_length",h4("Input: Tweet Character Count"),min=1,max=400, value=120),
                         numericInput("treepred_retweetcount",h4("Input: Retweet Count"),min=1,max=30000, value=120),
                         numericInput("treepred_favocount",h4("Input: Favorite Count"),min=1,max=30000, value=120),
                         tags$text(h4("Output: Most likely Media:")),
                         uiOutput("treepred")
                       )
              )
      ),
      # Logistic Regression Page
      tabItem(tabName = "model2",
              div(HTML("<em><h2>Logistic Regression Model</em>")),
              fluidRow(
                box(
                  tags$text(h3("Model Training")),
                  tags$text(h4("Model Design (MathJax Format)")),
                  uiOutput("ex3"),
                  tags$text(h4("Response: If the Media is CNN")),
                  selectInput("regression_var",h4("Input: Please select Predictor(s)"),choices = model2_choice, multiple = TRUE, selected = "retweet_count"),
                  tags$text(h4("Predictor Selected:")),
                  verbatimTextOutput("selected"),
                  withMathJax(),
                  tags$text(h4("Model Summary:")),
                  verbatimTextOutput("regression"),
                  numericInput("threshold_1",h4("Please Select Likelihood Threshold:"),min=0,max=1, value=0.3,step = 0.05),
                  tableOutput("fitresult")
                ),
                box(
                  tags$text(h3("New Data Fit")),
                  tags$text(h3("Please input values for trained predictor(s) ONLY")),
                  numericInput("logitpred_timing",h4("Input: Timing of the day (minutes)"), min=1, max=60*24, value=100,step=60),
                  numericInput("logitpred_display_text_width",h4("Input: Tweet Character Count"),min=1,max=400, value=100,step=50),
                  numericInput("logitpred_retweet_count",h4("Input: Retweet Count"),min=1,max=30000, value=100,step=100),
                  numericInput("logitpred_favorite_count",h4("Input: Favorite Count"),min=1,max=30000, value=100,step=100),
                  tags$text(h3("Input Summary: (variables not in model training will be ignored)")),
                  tableOutput("valueinput"),
                  tags$text(h3("Likelihood of Being a CNN Tweet:")),
                  textOutput("logitpred")
                )
              )
      ),
      # More on data page
      tabItem(tabName = "data",
              div(HTML("<em><h2>More On Data</em>")),
              fluidRow(
                column(12,
                     box(
                        tags$text(h3("Please Select Filters")),
                        selectInput("source", h3("Source of Tweet"),c(unique(tmls$source))),
                        selectInput("media", h3("Submission Media"),c(unique(tmls$screen_name))),
                        selectInput("isretw", h3("Is is a retweet?"),c(unique(tmls$is_retweet))),
                        numericInput("favcount", h3("Minimun Favoriate Count"), min = 1,max = 20000, value = 100),
                      )
                ),
                column(12,
                     box(
                       downloadLink("download_media_data", h2("Download Table Below")),
                       tableOutput("subset_table")
                     )
                )
              )
      )
    )
  )
)


# server file
server <- function(input, output,session) {

  # Request data from tweeter
  tmls <- get_timelines(c("cnn", "BBCWorld", "cnbc","msnbc"), n = 400)
  # Calculate for addtional varaibles
  tmls["timing_hour"] <- substr(tmls$created_at,12,13)
  tmls["timing_min"] <- substr(tmls$created_at,15,16)
  tmls["timing"] <- apply(tmls,1,function(data){60*as.numeric(data$timing_hour)+as.numeric(data$timing_min)})
  tmls["Media_CNN"] <- ifelse(tmls$screen_name=="CNN",1,0)
  # define default choice for modeling
  model2_choice <- c("select all","timing", "display_text_width","retweet_count","favorite_count")

  # Introduction Page, Box 1
  output$introtext1 <- renderUI({
    str1 <- h2("Data Description")
    str2 <- h4("The R package rtweet have built-in functionality to scrape data from twitter.")
    str3 <- h4("One interesting direction to look at twitter data is, to look at what tradistional
               medias are talking about during this Pandemic time. This file includes most recent 3,000
               twettes from each of the media in the list: CNN, BBCWorld, FoxNews, MSNBC. Fields
               includes, similarly, text, length, whether it's a retweet, hashtags, etc.")
    HTML(paste(str1, str2,str3, sep = '<br/>'))
  })
  # Introduction Page, Box 2
  output$introtext2 <- renderUI({
    str1 <- h2("Analysis Description")
    str2 <- h4("The first step of analysis is to explore the dataset we have. We can look at them by
               histrogram, bar-chart, plot, box-plot etc. The main goal of the exploration is to scratch
               the surface of internal relationship between varaibles, or simply to know more about
               the data sets we have. I performed very simple exploration on the datasets I have.")
    str3 <- h4("Second step is Cluster Analysis. This procedure can help us better understand the cluster of
               our data points. Basically, 4 numeric variables of this dataset were used for cluster analysis.")
    str4 <- h4("Third step is modeling. So regression model is a way that we tell the relationship between
               variables and response. The response could be a continous variable, binary variable, or categorical.
               The goal of regression modeling is to build a fit but not over-fit model that can explain
               the underlying relationship and/or make a good prediction. Another model used is Random Forest
               Model.")
    str5 <- h4("Fourth step / Last Step is sharing the source data for other researchers. This is an 
                important step for knowledge share and finding verification. Here, they can
                subset the dataset based on their interest.")    
    HTML(paste(str1, str2,str3,str4,str5, sep = '<br/>'))
  })
  # Introduction Page, Box 3
  output$introtext3 <- renderUI({
    str1 <- h2("APP Functionality")
    str2 <- h4("The functionality of this APP was built exactly following the flow of analysis: Starting
               from exploration step, in which user can save data and plot that they are exploring. And PCA
               analysis widget that user can specify aspects of the algorithm. In the modeling section,
               user can choose the variables at their will into the regression model. Also, after building the model,
               users are able to select values of predictors into the model and make a prediction. 
               For Random Forest, user can define parameters for model training. The last 
               section gives users an option to scroll through the data with their desired filters.")
    HTML(paste(str1, str2,sep = '<br/>'))
  })
  
  # Data Exploration, Simple Statistics for Numeric Variable
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
  # Data Exploration, Histogram for Numeric Variable
  output$stats_hist <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$numvarselect]]))
    plot + geom_histogram() +
      xlab(input$numvarselect)+
      facet_wrap(~screen_name)
   })
  # Data Exploration, bar chart for Numeric Variable
  output$binary_grap <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$binvarselect]]))
    plot + geom_bar( fill="steelblue") +
      xlab(input$binvarselect)+ 
      geom_text(aes(label=..count..,y=..count..+2),stat="count") + 
      facet_wrap(~screen_name)

  })
  # Data Exploration, bar chart for character: Source
  output$source_grap <- renderPlot({
    plot <- tmls %>% ggplot(aes(x=.data[[input$charvarselect]]))
    plot + geom_bar(fill="steelblue") +
      xlab(input$charvarselect)+ 
      geom_text(aes(label=..count..,y=..count..+2),stat="count") + 
      facet_wrap(~screen_name)
    
  })
  # Data Exploration, Table for character: text
  output$text_table <- renderTable({
    content <- tmls %>% select(screen_name,text)
    
    cnn_text <-   content %>% subset(screen_name=="CNN") %>% slice_head(n=20)
    bbc_text <-   content %>% subset(screen_name=="BBCWorld") %>% slice_head(n=20)
    cnbc_text <-   content %>% subset(screen_name=="CNBC") %>% slice_head(n=20)
    msnbc_text <-   content %>% subset(screen_name=="MSNBC") %>% slice_head(n=20)
    
    final <- rbind(cnn_text,bbc_text,cnbc_text,msnbc_text)
    final$text <- paste(substr(final$text,1,200),"...")
    final 
  })
  
  # Data Exploration, download handler for character: text
  output$downloadData <- downloadHandler(
    filename = "TweetData.csv",
    content = function(file) {
      write.csv(final, file)
    }
  )
  
  # Numeric Exploration Plot
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
  
  # More on data: Reactive Content on subset table
  subset_result <- reactive({
    subset_result <- tmls %>%filter(source==input$source) %>% filter(screen_name==input$media) %>%
                     filter(is_retweet==input$isretw)%>% 
                     filter(favorite_count>= input$favcount)
    subset_result <- subset_result[,c(4:7,12:14)]
  })
  
  # More on data: visualize Subset table
  output$subset_table <- renderTable({
    subset_result()
  })
  
  # More on data: Download handler for subset table
  output$download_media_data <- downloadHandler(
    file = "Subset Media Tweet.csv",
    content = function(file) {
      write.csv(subset_result(), file)
    }
  )
  
  # Cluster Analysis: Plot
  output$cluster_graph <- renderPlot({
    data_clustering <-  tmls %>% 
      group_by(screen_name) %>%
      sample_n(input$obs)
    
      hierClust <- hclust(dist(data.frame(data_clustering$timing, data_clustering$display_text_width, data_clustering$retweet_count,data_clustering$favorite_count)),
                          method=input$agglo)
      plotresult <- plot(hierClust,xlab="")
      plotresult
  })
  
  # Cluster Analysis: Download handler for plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("Cluster Download.png") },
    content = function(file) {
      ggsave(file, plot= plotresult, device = "png")
    }
  )
  # Random Forest; Subset for Tree data
  treedata <-  reactive({
    treedata <- tmls %>% 
    group_by(screen_name) %>%
    sample_n(input$treeobs) %>% 
    select(screen_name, timing, display_text_width,retweet_count,favorite_count)
  })
  # Random Forest: Core Model
  rfTree <- reactive({
    rfTree <- randomForest(as.factor(screen_name) ~ ., data = treedata(), mtry = input$vartry, ntree=input$treecount)
  })
  
  # Random Forest: summary
  output$randomforest_summary <- renderPrint({
    print(rfTree())
  })
  
  # Random Forest: Output plot
  output$randomforest <- renderPlot({
    plot(rfTree(), type="l",main = "Error Rate by Tree Count")
    legend("topright", legend=unique(treedata()$screen_name), col=1:4, pch=19)
  })
  
  # Randon Forest: Prediction Output
  output$treepred <- renderUI({
    newdata <- data.frame(timing=input$treepred_timing, display_text_width=input$treepred_length,
                          retweet_count=input$treepred_retweetcount,favorite_count=input$treepred_favocount)
    pre_rfTree <- predict(rfTree(), newdata)
    as.character(pre_rfTree)
  })
  
  # Logistic Regression: update selection
  observe({
    if("select all" %in% input$regression_var)
      selected_choices=model2_choice[-1]
    else
      selected_choices=input$regression_var
    updateSelectInput(session,"regression_var",selected = selected_choices)  
  })
  
  # Logistic Regression: Variable Selected
  output$selected <- renderText({
    paste(input$regression_var, collapse = ",")
  })
  
  # Logistic Regression: General Function
  output$ex3 <- renderUI({
    withMathJax(
      helpText('Using "Logit" as link function: 
               $$logit(Y=1|X) = {\\beta_0 + \\beta_1Predictor1 + \\beta_2Predictor2 + \\beta_3Predictor3...}\\!$$'))
  })
  
  # Logistic Regression: Core Model
  regresion_fit <- reactive({
    if("select all" %in% input$regression_var)
      selected_choices=model2_choice[-1]
    else
      selected_choices=input$regression_var
    step1 <- paste(selected_choices,collapse = "+")
    step2 <- paste("Media_CNN~",step1,collapse = "")
    user_defined_model <- as.formula(step2)
    fit <- glm(user_defined_model,data=tmls,family=binomial())
    fit
  })
  
  # Logistic Regression: Output Summary
  output$regression <- renderPrint({
    summary(regresion_fit())
  })
  
  # Logistic Regression: Dynamicly choose threshold
  output$fitresult <- renderTable({
    result <- table(as.numeric(predict(regresion_fit(), type="response") > input$threshold_1),tmls$Media_CNN)
    result <- as.data.frame(result)
    colnames(result) <- c("Predicted Result (CNN=1, Not CNN=2)","Reality Result (CNN=1, Not CNN=2)","Count of Observations")
    result
  })
  
  # Logistic Regression: Input data for prediction
  newdata2 <- reactive({
    newdata2 <- data.frame(timing=input$logitpred_timing, display_text_width=input$logitpred_display_text_width,
                           retweet_count=input$logitpred_retweet_count,favorite_count=input$logitpred_favorite_count)
  })
  # Logistic Regression: Show Input data for prediction
  output$valueinput <- renderTable({
    newdata2()
  })
  # # Logistic Regression: Prediction likelihood
  output$logitpred <- renderText({
    predict(regresion_fit(), newdata2(),type="response")
  })
}
