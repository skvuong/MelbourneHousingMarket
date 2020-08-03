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
          choices = c("Top N Suburbs By Count of Homes",
                      "Top N Suburbs by Average Price of Homes",
                      "House Price Distribution",
                      "Distance from Central District Distribution",
                      "No. of Bedrooms Distribution",
                      "No. of Bathrooms Distribution",
                      "No. of Car Lots Distribution",
                      "Price Distribution by Home Type",
                      "Price Distribution by Regions",
                      "Price Correlation Matrix"
                      ),
          selected =  "Top N Suburbs By Count of Homes"),
        sliderInput("numRegions", "No. of Suburbs in bar graph:", min=1,max= 30,value=10,step=1)
        ), 
    mainPanel(
      plotOutput(outputId = "plot1", height = "600px")
    )
  )
))