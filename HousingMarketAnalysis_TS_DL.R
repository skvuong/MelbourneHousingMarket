#--------------------------------------------------------------------------------------
################ Housing Market - Price Prediction using Neural Net ###################
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
## Install and load the required packages
#--------------------------------------------------------------------------------------
if(!require(astsa)) 
  install.packages("astsa")
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(fastDummies)) 
  install.packages("fastDummies")
if(!require(forecast)) 
  install.packages("forecast")
if(!require(corrplot)) 
  install.packages("corrplot")
if(!require(ggplot2)) 
  install.packages("ggplot2")
if(!require(ggthemes)) 
  install.packages("ggthemes")
if(!require(keras)) 
  install.packages("keras")
if(!require(magrittr)) 
  install.packages("magrittr")
if(!require(mlbench)) 
  install.packages("mlbench")
if(!require(neuralnet)) 
  install.packages("neuralnet")
if(!require(RColorBrewer)) 
  install.packages("RColorBrewer")
if(!require(sarima))
  install.packages("sarima")
if(!require(tidyr)) 
  install.packages("tidyr")
if(!require(tseries))
  install.packages("tseries")
if(!require(xts))
  install.packages("xts")

library(astsa)
library(dplyr)
library(fastDummies)
library(forecast)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(keras)
library(magrittr)
library(mlbench)
library(neuralnet)
library(RColorBrewer)
library(sarima)
library(tidyr)
library(tseries)
library(xts)

#--------------------------------------------------------------------------------------
## Loading Data
#--------------------------------------------------------------------------------------
housing_data <- read.csv("Melbourne_housing_FULL.csv", header=TRUE)

dim(housing_data)

colnames(housing_data)

str(housing_data)

#--------------------------------------------------------------------------------------
## Data Exploration and Visualization
#--------------------------------------------------------------------------------------

#Number of Unique Suburbs
length(unique(housing_data$Suburb))

#Number of Unique Regions
length(unique(housing_data$Regionname))

#List of Home Types
unique(housing_data$Type)

#1. Top 10 Suburbs By Count of Homes
#Calculate count of homes per suburb
top10 <- housing_data %>% group_by(Suburb) %>%
  summarise(Number = n()) %>% arrange(desc(Number)) %>%
  head(10)
#Plot
ggplot(top10, aes(reorder(Suburb, Number), Number, fill = Suburb))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Count of Homes",
       title = "Top 10 Suburbs by Count of Homes")+
  coord_flip()

#2. Top 10 Suburbs By Average Price
#Create new dataframe - price and suburb columns
SuburbandPrice <- housing_data[c("Suburb","Price")]
#Calculate average price per suburb
top10avgprice <- SuburbandPrice %>% group_by(Suburb) %>%
  summarise(Average = sum(Price)/n()) %>%
  arrange(desc(Average)) %>% head(10)
#Plot
ggplot(top10avgprice, aes(reorder(Suburb, Average), Average, fill = Suburb))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  labs(x = "Suburb", y = "Average Price Per Home",
       title = "Top 10 Suburbs by Average Price of Homes")

#3. Price Distribution of Homes
#Create new dataframe
DateandPrice <- housing_data[c("Suburb","Price","Date")] %>% na.omit()
#Plot
ggplot(DateandPrice, aes(Price))+
  geom_histogram(binwidth = 250000,color = "black", fill = "blue")+
  scale_x_continuous(breaks = c(1000000,2000000,3000000,4000000),
                     labels = c("$1m","$2m","$3m","$4m"))+
  ggtitle("Melbourne House Price Distribution")

#4. Distance from CBD Distribution
housing_data$Distance<- as.numeric(housing_data$Distance)
hist(housing_data$Distance, breaks = 40, xlim = c(0,50), ylim = c(0,3400),
     xlab = "Distance", col = "Blue", main = "Count of Distances from CBD", las =1)

#5. No. of Bedrooms Distribution
hist(housing_data$Bedroom2, breaks = 40, xlim = c(0,10), ylim = c(0,12000),
     xlab = "No. of Bedrooms", col = "grey", main = "Count of No. of Bedrooms", las =1)

#6. No. of Bathroom Distribution
hist(housing_data$Bathroom, breaks = 40, xlim = c(0,10), ylim = c(0,4000),
     xlab = "No. of Bathrooms", col = "Purple", main = "Count of No. of Bathrooms", las =1)

#7. No. of Car Lots Distribution
hist(housing_data$Car, breaks = 40, xlim = c(0,10), ylim = c(0,4000),
     xlab = "No. of Carslots", col = "Green", main = "Count of No. of Carslots", las =1)

#8. House Type Distribution
#Create New Dataframe
housedf2 <- housing_data %>%
  select(Price, Regionname,Type,Date)
#Plot
ggplot(housedf2, aes(Type, Price)) +
  geom_boxplot(outlier.colour = "blue") + 
  scale_x_discrete(labels = c('Houses','Townhouses','Units')) +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Home Type") +
  ylab("Price") +
  ggtitle("Home Type Price Distribution")

#9. Price Distribution of Regions
#Create New Dataframe
housedf2 <- housing_data %>%
  select(Price, Regionname,Type,Date)
#Plot
ggplot(housedf2, aes(Regionname, Price)) +
  geom_boxplot(outlier.colour = "blue") +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Region") +
  ylab("Price") +
  ggtitle("Price Distribution of Regions")

#10a. Correlation Matrix - All Homes
housedf3 <- housing_data %>%
  select(Rooms,Bedroom2,Bathroom,Distance,Car,Landsize,
         BuildingArea,YearBuilt,Propertycount,Price)
housedf3$Distance<- as.numeric(housedf3$Distance)
housedf3$Propertycount<- as.numeric(housedf3$Propertycount)
housedf3 <- na.omit(housedf3) 
corrplot(cor(as.matrix(housedf3)), method = "pie", type="lower")

#10b. Correlation Matrix - House Type Homes
housedfhome <- housing_data %>%filter(Type == "h") %>%
  select(Rooms,Bedroom2,Bathroom,Distance,Car,Landsize,
         BuildingArea,YearBuilt,Propertycount,Price)
housedfhome$Distance<- as.numeric(housedfhome$Distance)
housedfhome$Propertycount<- as.numeric(housedfhome$Propertycount)
housedfhome  <- na.omit(housedfhome ) 
corrplot(cor(as.matrix(housedfhome)), method = "pie", type="lower")

#10c. Correlation Matrix - Townhouse Type Homes
housedfhome <- housing_data %>%filter(Type == "t") %>%
  select(Rooms,Bedroom2,Bathroom,Distance,Car,Landsize,
         BuildingArea,YearBuilt,Propertycount,Price)
housedfhome$Distance<- as.numeric(housedfhome$Distance)
housedfhome$Propertycount<- as.numeric(housedfhome$Propertycount)
housedfhome  <- na.omit(housedfhome ) 
corrplot(cor(as.matrix(housedfhome)), method = "pie", type="lower")

#10d. Correlation Matrix - Unit Type Homes
housedfhome <- housing_data %>%filter(Type == "u") %>%
  select(Rooms,Bedroom2,Bathroom,Distance,Car,Landsize,
         BuildingArea,YearBuilt,Propertycount,Price)
housedfhome$Distance<- as.numeric(housedfhome$Distance)
housedfhome$Propertycount<- as.numeric(housedfhome$Propertycount)
housedfhome  <- na.omit(housedfhome ) 
corrplot(cor(as.matrix(housedfhome)), method = "pie", type="lower")

#--------------------------------------------------------------------------------------
## Time Series
#--------------------------------------------------------------------------------------
datam<-housing_data

#change data type as.numeric(), as.character(), as.vector(), as.matrix(), as.data.frame)
#data pretreatment
dataT<-select(datam,Price,Date,Type)
dataT$Price<-as.numeric(dataT$Price)
dataT<-na.omit(dataT)

dataT1<-dataT %>% group_by(Date,Type) %>% summarise(Mean_sales=mean(Price))
#dataT1<-aggregate(dataT&Price,by=list(dataT&Date,dataT&Type),FUN=mean)
#conditional select mf[ mf$a == 16, ]

dataT2 <- subset(dataT1, Type == "u")
#View(dataT2)

dataT3<-select(dataT2,Date,Mean_sales)
#change date type
#dataT3$Date<-as.POSIXct(dataT3$Date,format = "%d/%m/%Y")
dataT3$Date<-as.Date(dataT3$Date, format = "%d/%m/%Y")
#View(dataT3)

dataT4<-xts(dataT3$Mean_sales, as.Date(dataT3$Date, format="%d/%m/%Y"))
#View(dataT4)

#If too many outliers, it could be changed to log format but in this case, it is not needed
#LAP<-log(dataT4)
AP<-dataT4
plot(dataT4)

#Decomposition of additive time series
#decomp<-decompose(AP)
#plot(decomp$figure,type='b',xlab='Month',ylab='Seasonality Index',col='blue',las=2)
#plot(decomp)

#--------------------------------------------------------------------------------------
## Forecast
#--------------------------------------------------------------------------------------
#ARIMA- autoregressive integrated moving average
AP<-dataT4
model<-auto.arima(AP)
attributes(model)
model$coef

#ACF and PACF plots
#ACF plots display correlation between a series and its lag
acf(model$residuals,main='Correlogram')

#PACF plots display correlation between a series and its lag that explained by previous lag
pacf(model$residuals,main='Partial Correlogram')

#ljung-box test
Box.test(model$residuals, lag=20,type='Ljung-Box')

#residual plot
hist(model$residuals,col='red',xlab='Error',main='Histogram of Residuals',freq=FALSE)
lines(density(model$residuals))

#forecasr
f<-forecast(model,12)
autoplot(f)
accuracy(f)

#Forecasts using Exponential Smoothing  
skirtsseriesforecasts <- HoltWinters(AP, beta=FALSE,gamma=FALSE)
skirtsseriesforecasts$SSE
skirtsseriesforecasts2 <- forecast:::forecast.HoltWinters(skirtsseriesforecasts, h=12)
autoplot(skirtsseriesforecasts2)
accuracy(skirtsseriesforecasts2)

#Seasonal analysis 
seasonm<-sarima(AP, 1,0,0,0,1,1,12)

#sarima.for(AP, 24, 0,1,1,0,1,1,12)

#Time series parameter
plot(AP, type="b")

#diff12 = diff(AP,12)
#acf2(diff12, 48)

#final
models = arima(AP, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
predict(models, n.ahead=24) 
autoplot(forecast(models, 12))
accuracy(models)

#Extra code not related
#sarima(AP, 0,0,1,0,0,1,4)
#sarima(AP, 1,0,0,0,0,1,4)
#sarima(AP, 0,0,0,0,0,1,4)
#sarima(AP, 1,0,0,0,0,2,4)

#trunacate those under 0
themodel = arima(AP, order = c(1,0,0), seasonal = list(order = c(0,1,1), period = 12))
themodel
predict(themodel, n.ahead=12)
autoplot(forecast(themodel, n.ahead=12))

#monthly means to make the case that the data are seasonal.
flowm = matrix(AP, byrow=TRUE)
col.means=apply(flowm,2,mean)
plot(col.means,type="b", main="Monthly Means Plot for Flow", xlab="Month", ylab="Mean")

#seasonal analysis
models<-auto.arima(AP,seasonal=TRUE)
s<-forecast(models,12)
library(ggplot2)
autoplot(s)
accuracy(s)

#help(sarima)
models<-Arima(AP,order=c(0,1,2),seasonal=c(0,1,1))
s<-forecast(models,12)
library(ggplot2)
autoplot(s)
accuracy(s)

#fit<-stl(AP,s.wnidow="period")
#plot(fit)
#autoplot(s)
#accuracy(s)

#--------------------------------------------------------------------------------------
## DL Model - Data Cleanup 
#--------------------------------------------------------------------------------------
# Check for missing Price values and outliers
#--------------------------------------------------------------------------------------

# Check for missing Price values
sum(is.na(housing_data$Price))

# Check for outliers
housing_data %>% filter(Price < 200000) %>% nrow()
housing_data %>% filter(Price < 300000) %>% nrow()
housing_data %>% filter(Price < 400000) %>% nrow()

housing_data %>% filter(Price > 3000000) %>% nrow()
housing_data %>% filter(Price > 4000000) %>% nrow()
housing_data %>% filter(Price > 5000000) %>% nrow()
housing_data %>% filter(Price > 6000000) %>% nrow()

# Cleanup data
# Since our target variable is Price, we drop observations without Price value.
#--------------------------------------------------------------------------------------

# Start with full dataset
data_clean <- housing_data
dim(data_clean)

# Keep only records with non-null Price values
data_clean <- drop_na(data_clean, Price)
dim(data_clean)

# Keep only records with Price >= 300,000
data_clean <- data_clean %>% filter(Price >= 300000)
dim(data_clean)

# Keep only records with Price <= 5,000,000
data_clean <- data_clean %>% filter(Price <= 5000000)
dim(data_clean)

#--------------------------------------------------------------------------------------
## DL Modeling - Data Preparation
#--------------------------------------------------------------------------------------

# Divide the data into 10 buckets of equal sizes and assign Price Range value
num_buckets <- 10
data_clean$PriceRange <- as.numeric( cut_number( data_clean$Price, num_buckets) )

# Check the summary for the buckets
data_clean %>%
  group_by(PriceRange) %>%
  summarise(Count = n(), min = min(Price), max = max(Price), mean = mean(Price) )

str(data_clean)

#--------------------------------------------------------------------------------------
# DL Modeling - Features Selection
#
# The following attributes were identified as having an impact on 
# (are correlated to) the house price by our Data Analysis in section above.
#
# (Type, Rooms, Bedroom2, Bathroom, Car, Landsize, YearBuilt, BuildingArea, Distance)
#--------------------------------------------------------------------------------------

# Data Cleanup for Independent attributes
data_numeric <- data_clean %>%
  select(Rooms, Bedroom2, Bathroom, Car, Landsize, YearBuilt, BuildingArea, Distance, Price, PriceRange)

# Convert non-numeric columns with numeric data to numeric.
data_numeric$Distance <- as.numeric(data_numeric$Distance)
str(data_numeric$Distance)

# Fill nulls value in numeric columns with column mean values
col_means <- lapply(data_numeric, mean, na.rm = TRUE)
datac_na  <- replace_na(data_numeric, col_means)

# Double-check null value in numeric columns
summarise_all(datac_na, funs( sum(is.na(.)) ) )

# Add categorical attribute (Type) to column selection
#--------------------------------------------------------------------------------------
datac <- datac_na
datac$Type <- as.vector(data_clean$Type)

# Check distribution of categorical attribute (Type).
#--------------------------------------------------------------------------------------
datac %>%
  group_by(Type) %>%
  summarise(Count = n(), min = min(Price), max = max(Price), mean = mean(Price) )

# Create dummies for categorical attribute (Type).
#--------------------------------------------------------------------------------------
results <- fastDummies::dummy_cols(datac,select_columns='Type')

knitr::kable(results)

dataf <- results %>% select(Rooms, Bedroom2, Bathroom, Car, Landsize, YearBuilt, 
                            BuildingArea, Distance, Type_h,Type_t,Type_u, PriceRange)

dataf %<>% mutate_if(is.factor, as.numeric)

# Double-check null value in numeric columns
summarise_all(dataf, funs( sum(is.na(.)) ) )

# Create a simple Neuralnet
nn <- neuralnet(PriceRange ~ Rooms+Bedroom2+Bathroom+Car+Landsize+YearBuilt+BuildingArea+Distance+Type_h+Type_t+Type_u,
               data = dataf,
               hidden = c(10,5),
               linear.output = F,
               lifesign = 'full',
               rep=1)
plot(nn,
     col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

model_data <- as.matrix(dataf)
dimnames(model_data) <- NULL

# Use 80/20 Train/Test Split
#--------------------------------------------------------------------------------------
set.seed(123)
ind<-sample(2, nrow(model_data), replace=T, prob=c(0.8,0.2) )

train_set <- model_data[ind==1,1:8]
test_set  <- model_data[ind==2,1:8]

train_target <- model_data[ind==1,9]
test_target  <- model_data[ind==2,9]

#Normalization
mf <- colMeans(train_set)
sf <- apply(train_set, 2, sd)

train_set <- scale(train_set, center = mf, scale = sf)
test_set  <- scale(test_set, center = mf, scale = sf)

#Categorization
train_target = train_target -1
train_target_cat = to_categorical(train_target, num_classes = num_buckets)

test_target = test_target -1
test_target_cat  = to_categorical(test_target, num_classes = num_buckets)

#--------------------------------------------------------------------------------------
# Install tensorflow / keras packages
# Note those will take at least half hours
#--------------------------------------------------------------------------------------
#install.packages('devtools')
#library(devtools)

#devtools::install_github("rstudio/tensorflow")
#devtools::install_github("rstudio/keras")
#tensorflow::install_tensorflow()
#tensorflow::tf_config()

use_condaenv("r-tensorflow")

#--------------------------------------------------------------------------------------
# DL Modeling
#--------------------------------------------------------------------------------------
# Create a sequential model in Keras, by stacking the layers sequentially.
# - The model has 3 hidden layers.
# - Keras builds an implicit input layer using the input_shape parameter.
# - Last layer is the output layer.
#--------------------------------------------------------------------------------------
model <- keras_model_sequential()
model %>% 
  layer_dense(units  = 100, activation = 'relu', input_shape = c(8) ) %>%
  layer_dense(units  = 50, activation = 'relu')  %>%
  layer_dense(units  = 20, activation = 'relu')  %>%
  layer_dense(units  = ncol(train_target_cat) )

# Take a look at the model and the shapes of the layers:
model

# Define the loss and optimizer functions and the metric to optimize.
compile(model, loss = 'categorical_crossentropy',
        optimizer = optimizer_rmsprop(), metrics = "accuracy")

# Fit (train) the model
model_hist <- fit(model, train_set, train_target_cat,
                  epochs=100, batch_size=32, validation_split=0.2)

# Plot fitting history
plot(model_hist)

#--------------------------------------------------------------------------------------
## Evaluating the model
#--------------------------------------------------------------------------------------
## Using Testset data
evaluate(model, test_set, test_target_cat)

#--------------------------------------------------------------------------------------
# Validate Model with unseen data
#--------------------------------------------------------------------------------------

# Predict new cases
test_pred <- predict_classes(model, test_set)

pred_prob <- predict_proba(model, test_set)

# Confusion matrix
confusion_matrix = table(Predicted = test_pred, Actual = test_pred)
confusion_matrix

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
