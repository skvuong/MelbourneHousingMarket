#-------------------------------------------------------------------------------
#server.R
#-------------------------------------------------------------------------------

if(!require(dplyr))
  install.packages("dplyr")
if(!require(ggthemes))
  install.packages("ggthemes")
if(!require(ggplot2))
  install.packages("ggplot2")
if(!require(corrplot))
  install.packages("corrplot")

#Read CSV
#-------------------------------------------------------------------------------
housedata <- read.csv("Melbourne_housing_FULL.csv",header = TRUE)


#-------------------------------------------------------------------------------
# Define server logic required to visualization
#-------------------------------------------------------------------------------
server <- function(input, output) {
  output$plot1 <- reactivePlot(function() {

# Plotting chart based on input option
#-------------------------------------------------------------------------------


#1. Top N Suburbs By Count of Homes
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Top N Suburbs By Count of Homes") {
#Calculate count of homes per suburb
top_n <- housedata %>% group_by(Suburb) %>%
  summarise(Number = n()) %>% arrange(desc(Number)) %>%
  head(input$numRegions)
#Plot
p <- ggplot(top_n, aes(reorder(Suburb, Number), Number, fill = Suburb))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Count of Homes",
       title = "Top N Suburbs by Count of Homes")+
  coord_flip()
}


#2. Top N Suburbs By Average Price
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Top N Suburbs by Average Price of Homes") {
#Create new dataframe - price and suburb columns
SuburbandPrice <- housedata[c("Suburb","Price")]
#Calculate average price per suburb
top_n_avgprice <- SuburbandPrice %>% group_by(Suburb) %>%
  summarise(Average = sum(Price)/n()) %>%
  arrange(desc(Average)) %>% head(input$numRegions)
#Plot
p <- ggplot(top_n_avgprice, aes(reorder(Suburb, Average), Average, fill = Suburb))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Average Price Per Home",
       title = "Top N Suburbs by Average Price of Homes")
}


#3. House Price Distribution
#-------------------------------------------------------------------------------
if (input$visualizeOption == "House Price Distribution") {
#Create new dataframe
DateandPrice <- housedata[c("Suburb","Price","Date")] %>% na.omit() %>% filter(Price < 7000000)
#Plot
p <- ggplot(DateandPrice, aes(Price))+
  geom_histogram(binwidth = 250000,color = "black", fill = "blue")+
  scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000,5000000,6000000,7000000),
                     labels = c("$1m","$2m","$3m","$4m","$5m","$6m","$7m"))+
  ggtitle("House Price Distribution")
}


#4. Distance from Central District Distribution
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Distance from Central District Distribution") {
housedata$Distance<- as.numeric(housedata$Distance)
p <- hist(housedata$Distance, breaks=40, xlim=c(0,50), ylim=c(0,3400),
     xlab="Distance", col="Blue", main ="Distance from Central District Distribution", las=1)
}


#5. No. of Bedrooms Distribution
#-------------------------------------------------------------------------------
if (input$visualizeOption == "No. of Bedrooms Distribution") {
p <- hist(housedata$Bedroom2, breaks=40, xlim=c(0,10), ylim=c(0,12000),
     xlab="No. of Bedrooms", col="Red", main="No. of Bedrooms Distribution", las=1)
}


#6. No. of Bathrooms Distribution
#-------------------------------------------------------------------------------
if (input$visualizeOption == "No. of Bathrooms Distribution") {
p <- hist(housedata$Bathroom, breaks=40, xlim=c(0,10), ylim=c(0,4000),
     xlab="No. of Bathrooms", col="Purple", main="No. of Bathrooms Distribution", las=1)
}


#7. No. of Car Lots Distribution
#-------------------------------------------------------------------------------
if (input$visualizeOption == "No. of Car Lots Distribution") {
p <- hist(housedata$Car, breaks=40, xlim=c(0,10), ylim=c(0,4000),
     xlab="No. of Carslots", col="Green", main="No. of Car Lots Distribution", las=1)
}


#8. Price Distribution by Home Type
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Price Distribution by Home Type") {
#Create New Dataframe
housedf2 <- housedata %>%
  select(Price, Regionname,Type,Date)
#Plot
p <- ggplot(housedf2, aes(Type, Price)) +
  geom_boxplot(outlier.colour = "blue") + 
  scale_x_discrete(labels = c('Houses','Townhouses','Units')) +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Home Type") +
  ylab("Price") +
  ggtitle("Price Distribution by Home Type")
}


#9. Price Distribution of Regions
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Price Distribution by Regions") {
#Create New Dataframe
housedf2 <- housedata %>%
  select(Price, Regionname,Type,Date)
#Plot
p <- ggplot(housedf2, aes(Regionname, Price)) +
  geom_boxplot(outlier.colour = "blue") +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Region") +
  ylab("Price") +
  ggtitle("Price Distribution by Regions")
}


#10. Price Correlation Matrix - All Homes
#-------------------------------------------------------------------------------
if (input$visualizeOption == "Price Correlation Matrix") {
housedf3 <- housedata %>%
  select(Rooms,Bedroom2,Bathroom,Distance,Car,Landsize,BuildingArea,YearBuilt,Propertycount,Price)
housedf3$Distance<- as.numeric(housedf3$Distance)
housedf3$Propertycount<- as.numeric(housedf3$Propertycount)
housedf3 <- na.omit(housedf3) 
p <- corrplot(cor(as.matrix(housedf3)), method = "pie", type="lower")
}

#Plot the chart
#-------------------------------------------------------------------------------
print(p)
}
)}