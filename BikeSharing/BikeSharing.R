#############################################################
# Load the data sets from my GitHub link
#############################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(sqldf)) install.packages("sqldf", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
# Loading all needed libraries

library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(caret)
library(corrplot)

bike_share_train <- read.csv(
  "https://raw.githubusercontent.com/sabinychungath/Bike-Sharing/master/train.csv", 
  header = TRUE)
bike_share_test <- read.csv(
  "https://raw.githubusercontent.com/sabinychungath/Bike-Sharing/master/test.csv",
  header = TRUE)

#List first few lines of bike sharing dataset
head(bike_share_train)
head(bike_share_test)

#Type of bike sharing dataset
class(bike_share_train)

#Variables data types
str(bike_share_train)
colnames(bike_share_train)

# summary statistics
summary(bike_share_train)

#############################################
## Data Cleaning
#############################################
# Ignore the casual, registered fields as this sum is equal to count field
bike_share_train <- bike_share_train[,-c(10,11)]

# Check for NA values
#Missing values in dataset
missing_value <- data.frame(apply(bike_share_train,2,
                                  function(x)
                                  {
                                    sum(is.na(x))
                                  }
))
names(missing_value)[1]='missing_value'
missing_value

# Converting integer to factor on training set
bike_share_train$season <- as.factor(bike_share_train$season)
bike_share_train$holiday <- as.factor(bike_share_train$holiday)
bike_share_train$workingday <- as.factor(bike_share_train$workingday)
bike_share_train$weather <- as.factor(bike_share_train$weather)
bike_share_train$datetime <-as.POSIXct(bike_share_train$datetime, format="%Y-%m-%d %H:%M:%S")

# Converting int to factor on test set
bike_share_test$season <- as.factor(bike_share_test$season)
bike_share_test$holiday <- as.factor(bike_share_test$holiday)
bike_share_test$workingday <- as.factor(bike_share_test$workingday)
bike_share_test$weather <- as.factor(bike_share_test$weather)
bike_share_test$datetime <-as.POSIXct(bike_share_test$datetime, format="%Y-%m-%d %H:%M:%S")

# Extract day from datetime value
bike_share_train$day <-  strftime(bike_share_train$datetime, '%u')
bike_share_train$day <- as.factor(bike_share_train$day)
bike_share_test$day <-  strftime(bike_share_test$datetime, '%u')
bike_share_test$day <- as.factor(bike_share_test$day)

# Extract hour from datetime value
bike_share_train$hour <- substring(bike_share_train$datetime, 12,13)
bike_share_train$hour <- as.factor(bike_share_train$hour)
bike_share_test$hour <- substring(bike_share_test$datetime, 12,13)
bike_share_test$hour <- as.factor(bike_share_test$hour)

#Let's plot the hourly trend of count over hours and check if our hypothesis is correct or not.
#We will separate train and test data set from combined one.
boxplot(bike_share_train$count ~ bike_share_train$hour,xlab="hour", ylab="count of users")


# Removing datetime field 
bike_share_train <- bike_share_train[,-1]

#list first few lines of train & test dataset after cleaning
head(bike_share_train)
head(bike_share_test)

##################################################
## Data Visualization
##################################################
library(sqldf)
library(ggplot2)
# Get the average count of bikes rent by season, hour
season_hour <- sqldf('select season, hour, avg(count) as count from bike_share_train group by season, hour')
#Plot average count of bikes rent by season & hour
bike_share_train %>%
  ggplot(aes(x=hour, y=count, color=season)) +
  geom_point(data = season_hour, aes(group = season)) +
  geom_line(data = season_hour, aes(group = season)) +
  ggtitle("Bikes Rent By Season") + 
  scale_colour_hue('Season',breaks = levels(bike_share_train$season), 
                   labels=c('spring', 'summer', 'fall', 'winter'))

# Get the average count of bikes rent by weather, hour
weather_hour <- sqldf('select weather, hour, avg(count) as count from bike_share_train group by weather, hour')
#Plot average count of bikes rent by weather & hour
bike_share_train %>%
  ggplot(aes(x=hour, y=count, color=weather)) +
  geom_point(data = weather_hour, aes(group = weather)) +
  geom_line(data = weather_hour, aes(group = weather)) +
  ggtitle("Bikes Rent By Weather") + 
  scale_colour_hue('Weather',breaks = levels(bike_share_train$weather), 
                   labels=c('Good', 'Normal', 'Bad', 'Very Bad'))

#average count of bikes rent by day & hour
day_hour <- sqldf('select day, hour, avg(count) as count from bike_share_train group by day, hour')
#Plot average count of bikes rent by day & hour
bike_share_train %>%
  ggplot(aes(x=hour, y=count, color=day)) +
  geom_point(data = day_hour, aes(group = day)) +
  geom_line(data = day_hour, aes(group = day)) +
  ggtitle("Bikes Rent By day") + 
  scale_colour_hue('Day',breaks = levels(bike_share_train$day), 
                   labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

library(funModeling)
# Plotting Numerical bike_sharing
plot_num(bike_share_train)

######################################################
##Correlation Analysis
######################################################
# Correlation plot between fields
# It shows that temp, atemp has much correlation
bike_share_train_subset <- bike_share_train[,5:9]
bike_share_train_subset$humidity <- as.numeric(bike_share_train_subset$humidity)
bike_share_train_subset$count <- as.numeric(bike_share_train_subset$count)

train_cor <- cor(bike_share_train_subset)
corrplot(train_cor, method = 'color', addCoef.col="black")

#----------------------------
##Splitting the Train dataset
#----------------------------
library(caTools)
set.seed(123)
split <- sample.split(bike_share_train$count, SplitRatio = 0.75)
training_set <- subset(bike_share_train, split == TRUE)
validation_set <- subset(bike_share_train, split == FALSE)

#######################################################
# Model Development
#######################################################
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#------------------------------
##K Nearest Neighbor(KNN) Model
#------------------------------
#Training the model
knn_model <- train(count ~ ., method = "knn",data = training_set)

# Apply prediction on validation set
knn_predict <- predict(knn_model, newdata = validation_set)
print("summary of KNN prediction")
summary(knn_predict)

library(Metrics)
#Root-mean-square error value between actual and predicted
knn_RMSE <- RMSE(validation_set$count,knn_predict)
print("RMSE value between actual and predicted")
knn_RMSE

#If we want to penalize under-prediction of count, rmsle might be a better metric
knn_log_RMSE<-rmsle(validation_set$count,knn_predict)
print("Log RMSE value")
knn_log_RMSE

#Residual plot vs Fitted plot
y_test <- validation_set$count
residuals <- y_test - knn_predict
plot(y_test,residuals,
     xlab='Observed',
     ylab='Residuals',
     main='Residual plot')
abline(0,0)

#-----------------------------------
##SVM - Support Vector Machine Model
#-----------------------------------
library(e1071)
#Training the model
svm_model <- svm(count ~ ., data = training_set, kernel='sigmoid')

# Apply prediction on validation set
svm_predict <- predict(svm_model, newdata = validation_set)
print("summary of SVM prediction")
summary(svm_predict)

#Root-mean-square error value between actual and predicted
svm_RMSE<-RMSE(validation_set$count,svm_predict)
print("RMSE value between actual and predicted")
svm_RMSE

#From above summary we saw negative values of predicted count.
#We don't want negative values as forecast for bike count. Replace all negative numbers with 1
#Replace all negative numbers with 1
Output_svmMod <- svm_predict
Output_svmMod[svm_predict<=0] <- 1

#If we want to penalize under-prediction of demand, rmsle might be a better metric
svm_log_RMSE<-rmsle(validation_set$count,Output_svmMod)
print("RMSE value after replaced the negative values")
svm_log_RMSE

#Residual plot
y_test <- validation_set$count
residuals <- y_test - svm_predict
plot(y_test,residuals,
     xlab='Observed',
     ylab='Residuals',
     main='Residual plot')
abline(0,0)

#--------------------------
## Linear Regression model
#--------------------------
#Training the model
lm_model <- lm(count~., data = training_set)

#Stepwise Model Selection
# Now performs stepwise model selection by AIC with both directions(Forward, Backward)
library(MASS)
lm_model_AIC<-stepAIC(lm_model, direction="both")

# Apply prediction on validation set
lm_predict <- predict(lm_model_AIC, newdata = validation_set)
print("summary of lm prediction")
summary(lm_predict)

#RMSE value between actual and predicted
lm_RMSE<-RMSE(validation_set$count,lm_predict)
print("RMSE value between actual and predicted")
lm_RMSE

#From above summary we saw negative values of predicted count.
#We don't want negative values as forecast for bike count. Replace all negative numbers with 1
#Replace all negative numbers with 1
Output_lmMod <- lm_predict
Output_lmMod[lm_predict<=0] <-1

#If we want to penalize under-prediction of demand, rmsle might be a better metric
lm_log_RMSE<-rmsle(validation_set$count,Output_lmMod)
print("RMSE value after replaced the negative values")
lm_log_RMSE

#Residual plot vs Fitted plot
y_test <- validation_set$count
residuals <- y_test - lm_predict
plot(y_test,residuals,
     xlab='Observed',
     ylab='Residuals',
     main='Residual plot')
abline(0,0)

#--------------------
#Random Forest Model
#--------------------
library(randomForest)
#training the model
rf_model<-randomForest(count~. ,data = training_set,importance=TRUE,ntree=200)

# Apply prediction on validation set
rf_predict <- predict(rf_model, newdata = validation_set)
print("summary of random forest prediction")
summary(rf_predict)

#RMSE value between actual and predicted
rf_RMSE <- RMSE(validation_set$count,rf_predict)
print("RMSE value between actual and predicted")
rf_RMSE

#If we want to penalize under-prediction of demand, rmsle might be a better metric
rf_log_RMSE<-rmsle(validation_set$count,rf_predict)
print("log RMSE value")
rf_log_RMSE

#Residual plot
y_test <- validation_set$count
residuals <- y_test - rf_predict
plot(y_test,residuals,
     xlab='Observed',
     ylab='Residuals',
     main='Residual plot')
abline(0,0)


################################################################
# Results
################################################################
RMSE_results <- data.frame(Method = c("K Nearest Neighbor(KNN) Model",
                                      "Support Vector Machine (SVM) Model",
                                      "Linear Regression Model", 
                                      "Random Forest Model"), 
                           RMSE = c(knn_RMSE, svm_RMSE, lm_RMSE, rf_RMSE),
                           rmsle = c(knn_log_RMSE, svm_log_RMSE, lm_log_RMSE, rf_log_RMSE))

RMSE_results 