### ST 558 Project 3 ###
### Author: Xinyu Hu ###
### Date 7/27/2020   ###
########################

# Load library
library(shinydashboard)
library(shiny)
library(plotly)

ui <- dashboardPage(
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

