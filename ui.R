library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Baseball Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("th")),
      menuItem("PCA Analysis", tabName = "pca", icon = icon("th")),
      menuItem("Modeling", tabName = "model", icon = icon("th")),
      menuItem("More On Data", tabName = "data", icon = icon("th"))
    )
  )
  ,
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro", h2("Introduction")
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
