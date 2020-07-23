
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
      tabItem(tabName = "explore",h2("Data Exploration")
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
