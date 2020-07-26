
library(shinydashboard)
library(shiny)
library(plotly)
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title = "Baseball Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("table")),
      menuItem("Numeric Exploration", tabName = "explore_num", icon = icon("table")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("bar-chart-o")),
      menuItem("Modeling", tabName = "model", icon = icon("bar-chart-o")),
      menuItem("More On Data", tabName = "data", icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    tabItems(
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
      tabItem(tabName = "explore",
              div(HTML("<em><h2> Data Exploration</em>")),
              fluidRow(
                box(
                  # selectInput("mediapick","Select Source Media", c(CNN="cnn", BBC="BBCWorld", FOX="foxnews",MSNBC="msnbc",All="all")),
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
      tabItem(tabName = "explore_num",
              div(HTML("<em><h2>Numeric Exploration</em>")),
              box(
                tags$text(h3("Explore Relationship Between Numeric Varaibles")),
                selectInput("x_axis_select", h3("Select X-Axis Variable"),c("display_text_width","favorite_count","retweet_count")),
                selectInput("y_axis_select", h3("Select Y-Axis Variable"),c("display_text_width","favorite_count","retweet_count")),
                plotlyOutput("plotly_plot")
              )
      ), 
        
      tabItem(tabName = "pca",
              div(HTML("<em><h2> PCA Analysis</em>")),
              box(
                selectInput("agglo", h3("Select Agglomeration Method"),c("ward.D", "ward.D2", "single", "complete", "average","mcquitty", "median", "centroid")),
                numericInput("obs", h3("Number of Tweets (each Media) for Clustering"), min = 1,max = 100, value = 20),
                downloadButton('downloadPlot',"Download Plot"),
                plotOutput("cluster_graph")
              )
        
      ),
      tabItem(tabName = "model",
              div(HTML("<em><h2> Modeling</em>")),
              fluidRow(
                column(12,
                       box(
                         tags$text(h3("Random Forest Model")),
                         tags$text(h4("Response: Media")),
                         tags$text(h4("Predictor: Tweet Timing, Text Width, Retweet Count, Favorite_Count")),
                         numericInput("treecount", h4("Select Number of Trees to Grow"), min = 1,max = 200, value = 5),
                         numericInput("vartry", h4("Select Number of Variables randomly tried at each node"), min = 1,max = 4, value = 1),
                         numericInput("treeobs", h4("Number of Tweets (each Media) for Random Forest"), min = 1,max = 200, value = 10),
                         renderPlot("randomforest")
                       )
                )
              )
      ),
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

