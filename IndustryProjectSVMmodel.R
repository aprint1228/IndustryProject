#Industry Project - SVM Model 
# Last Updated: 01/20/2021
#--------------------------------------------------------
#Setting working directory and using read.delim library to read the Bus data file
setwd("/Users/kavasseris/OneDrive/DataBootcamp/Files/")
#--------------------------------------------------------
#Setting working directory and load libraries
library(readxl)
library(class)
library(e1071) #library to support SVM analysis
library(Metrics) # library to support analysis of metrics
library(MLmetrics) # lbrary to support analysis of metrics
#Import Data and clean data
#----------------------------------------
waitdataRaw<- read_excel("waitdata.xlsx", 1)
#Now lets lake a look @ summary
summary(waitdataRaw)
head(waitdataRaw)
tail(waitdataRaw)
##visually inspect wait time in raw dataset
hist(waitdataRaw$DelayCount, breaks=100)
plot(waitdataRaw$AheadCount, waitdataRaw$Wait)
colnames(waitdataRaw)
#visually inspect wait time
hist(waitdataRaw$Wait, breaks = 100)
#We can see some outliers
waitdataRemOutliers<-subset(waitdataRaw, waitdataRaw$Wait>-201)
waitdataRemOutliers<-subset(waitdataRaw, waitdataRaw$Wait<250)
# lets visually inspect wait time again
hist(waitdataRemOutliers$Wait, breaks = 100)
#further subset the data to only include the predictor feature "wait" (dependent variable -y) and  set of 23 features (independent variables-x)
waitSelectedDataset <- waitdataRemOutliers[, c(4,8,21,9,69,73,83,81,84,86,89,92,54,84,69,7,93,25,27,28,29,30,31,32,66)]
#Prepare Test and Train data
#----------------------------------------
# Generate test data  of 75% sample size that is random from the waitdata dataframe
#1.	Use the 75% of the data to train the SVM Model and use the remaining 25% for testing. 
set.seed(123)
trainwaitdataRowsSelected <- sample(1:nrow(waitSelectedDataset),0.75 * nrow(waitSelectedDataset))
# build the train dataset comprised of 75% of wait data
trainwaitdataset <-waitSelectedDataset[trainwaitdataRowsSelected,]
#build the test dataset comprised of remaining 25% of wait data
testwaitdataset<- waitSelectedDataset[-trainwaitdataRowsSelected,]
#-----------------------------------------------------------------
#Now, apply SVM Model (Regression Approach) on the train dataset
#-----------------------------------------------------------------
# List of Selected features that can be used as input to the model:
# DelayCount+AheadCount+InProgressSize+NumCompletedToday+AvgDelayForDay
#+OutpatientWaitingCount+NumAddOnsToday
#+DelayedInLine+LineCount0Strict
#+WithAndWithoutContrastCountWaiting+WithContrastCountWaiting
#+NumScannersUsedToday+DayOfWeek
#+NeuroCount+AbdominalCount+VascularCount+CardiacCount+MSKCount+ThoracicCount
#+NumScheduledNextW2+AvgWaitLastW2
#-----------------------------------------------------------------
wait_time_svm_model <-svm(Wait~DelayCount+InProgressSize+NumCompletedToday+AvgDelayForDay
                         +NumAddOnsToday+AheadCount+OutpatientWaitingCount
                         +WithContrastCountWaiting+WithAndWithoutContrastCountWaiting
                         +DelayedInLine+NumScheduledNextW2+InProgressSize+LineCount0Strict
                         ,data = trainwaitdataset)
summary(wait_time_svm_model) 
#predict wait tines using the new SVM model
predict_wait <- predict(wait_time_svm_model,testwaitdataset)
summary(predict_wait)
#store results in a variable
svm_result <- data.frame(testwaitdataset$Wait)
svm_result <- cbind(svm_result,data.frame(predict_wait))
names(svm_result)[1] <- "Actual_Wait_Time"
names(svm_result)[2] <- "Predicted_Wait_Time"
#now analyze the prediction model performance via different metrics
#1. Mean Square Error
error_mse <- mse(testwaitdataset$Wait,predict_wait)
print(error_mse)
#2. Root Mean Square Error
error_rmse <- RMSE(y_pred=predict_wait,testwaitdataset$Wait)
print(error_rmse)
#End of SVM Model



