
library(shinydashboard)
library(shiny)
ui <- dashboardPage(
  skin="red",
  dashboardHeader(title = "Baseball Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("table")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("bar-chart-o")),
      menuItem("Modeling", tabName = "model", icon = icon("bar-chart-o")),
      menuItem("More On Data", tabName = "data", icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h2("Introduction"),
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
      tabItem(tabName = "explore",h2("Data Exploration"),
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
                              c("is_quote","is_retweet"))
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
                  )
                )
              )
      ),
      tabItem(tabName = "pca",h2("PCA Analysis")
      ),
      tabItem(tabName = "model",h2("Modeling")
      ),
      tabItem(tabName = "data",h2("More On Data")
      )
    )
  )
)

