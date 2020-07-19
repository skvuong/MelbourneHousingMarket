#-------------------------------------------------------------------------------
#ui.R
#-------------------------------------------------------------------------------

if(!require(shiny))
  install.packages("shiny")
if(!require(shinythemes))
  install.packages("shinythemes")

# Define UI for application
#-------------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    "Housing Market Analysis",id = "inTabset",
    tabPanel("Analysis", icon = icon("chart-bar"),
      sidebarPanel(
        selectInput(inputId = "visualizeOption",
          label = "Housing Market - Visualization:",
          choices = c("Top 10 Suburbs By Count",
                      "Price Correlation Matrix"),
          selected = "Top 10 Suburbs By Count")
        ), 
    mainPanel(
      plotOutput(outputId = "plot1", height = "300px")
    )
  )
))