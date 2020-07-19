#-------------------------------------------------------------------------------
#server.R
#-------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(wordcloud)
library(plotly)
library(ggplot2)
library(stringr)
#library(plyr)
#library(tidyr)
library(dplyr)
library(datasets)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(gmodels)
library(corrplot)
library(neuralnet)

# Loading data
#-------------------------------------------------------------------------------
top10_suburbs      <- readRDS("top10_suburbs.rds")
correlation_matrix <- readRDS("correlation_matrix.rds")

# Define server logic required to visualization
#-------------------------------------------------------------------------------
server <- function(input, output) {
    #====Main page plot====
    output$plot1 <- reactivePlot(function() {
    # check for the input option
    if (input$visualizeOption == "Top 10 Suburbs By Count") {
        p <- ggplot(top10_suburbs, aes(reorder(Suburb,Number), Number, fill=Suburb))+
            geom_bar(stat = "identity")+
            theme(legend.position = "none")+
            labs(x = "Suburb", y = "Count of Homes", title = "Top 10 Suburbs by Count of Homes")
    }
    else if (input$visualizeOption == "Price Correlation Matrix"){
        p <- corrplot(correlation_matrix, method="pie", type="lower", order="hclust",
                      col=brewer.pal(n=8, name="RdYlBu"))
    }
    print(p)
    }
)
}