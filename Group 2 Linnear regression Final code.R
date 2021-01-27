library(Hmisc)
library(psych)
library(caret)
library(ggplot2)
library(rgl)
Wait_time<-read.csv("Wait_Time.csv")
colnames(Wait_time)
head(Wait_time)
summary(Wait_time)
#rename "wait"
names(Wait_time)[1]<-"wait"
colnames(Wait_time)

## a new column will be created for number of appointments today" 
library(lubridate)


Wait_time$parsedappt<- parse_date_time(Wait_time$x_ScheduledDTTM, 'y-m-d H:M:S')

Wait_time$calendar<- date(Wait_time$parsedappt)
Wait_time$calendar<- as.character(Wait_time$calendar)
Wait_time$numberapptstoday<- as.numeric(ave(Wait_time$calendar, Wait_time$calendar, FUN = length))


#Elimate missing values  NA 
sum(is.na(Wait_time))

#relevant columns, clean up 
Wait_time<-(Wait_time[,c(1,5:89,107)])
colnames(Wait_time)

#missing data NA 
sum(is.na(Wait_time))


#plot wait time 
hist(Wait_time$wait, breaks= 1000)
#We can see some outliers lets take negetive wait time of 150mins it seems too early
Wait_time<-subset(Wait_time, Wait_time$wait>-150)
#lets take NumCompletedToday assuming we complete at least one a day.
Wait_time<-subset(Wait_time, Wait_time$NumCompletedToday>0)


colnames(Wait_time)




#Transform date format

set.seed(5)
ran<- sample(1:nrow(Wait_time),.75*nrow(Wait_time))
model_data<-c("wait","DelayCount","AheadCount","NumCompletedToday","AvgDelayForDay",
              "NumAddOnsToday","OutpatientWaitingCount","WithContrastCountWaiting",
              "DelayedInLine","NumScheduledNextW2","InProgressSize","LineCount0Strict", "WithAndWithoutContrastCountWaiting",
              "ThoracicCount","NeuroCount","AbdominalCount","VascularCount","CardiacCount",
              "MSKCount","NumScannersUsedToday","DayOfWeek", "numberapptstoday")


waitdata_train<- Wait_time[ran,c(model_data)]
waitdata_test<- Wait_time[-ran,c(model_data)]


model<-lm(wait~DelayCount+AheadCount+InProgressSize+NumCompletedToday+AvgDelayForDay+
             NumAddOnsToday+OutpatientWaitingCount+WithContrastCountWaiting+
             DelayedInLine+NumScheduledNextW2+InProgressSize+LineCount0Strict+
             ThoracicCount+NeuroCount+AbdominalCount+VascularCount+CardiacCount+
             MSKCount+NumScannersUsedToday+DayOfWeek+ numberapptstoday,data = waitdata_train)
summary(model)

#The scale residual is little skewered due to 
#the presence of high negative wait time in the data set.residuals of this model is ok
plot(model)

# Test Fit
model.Test<-lm(wait~DelayCount+AheadCount+InProgressSize+NumCompletedToday+AvgDelayForDay+
             NumAddOnsToday+OutpatientWaitingCount+WithContrastCountWaiting+
             DelayedInLine+NumScheduledNextW2+InProgressSize+LineCount0Strict+
             ThoracicCount+NeuroCount+AbdominalCount+VascularCount+CardiacCount+
             MSKCount+NumScannersUsedToday+DayOfWeek+ numberapptstoday,data = waitdata_test)
summary(model.Test)
# Looks like the fit is good the variance adjusted R-squared for the model train and test is 0.003


summary(model)

plot(waitdata_train$AheadCount,waitdata_train$wait,col="slategray", 
     main="Ahead Count regression model with abline")
lm(wait~ AheadCount, data =waitdata_train)
abline(32.9119,-0.7261, col ="red")
#y=-6.96*x+13.38
# According to the model AheadCount has the biggest correlation
#An increase in AheadCount by one  induces a decrease in wait time by 6.48mins 

#y=-6.39*x+13.38
#Followed by NumScannersUsedToday, An increase in the number of scanners used in a day 
# decrease wait time by 7 mins
#
#y=5*x+13.38
#An increase in the delayed in line increases wait time by 18mins 

#predictions 
predictiction_data <- waitdata_train[1:3,]
predictiction_data$AheadCount<-2
predictiction_data$wait
pre<-predict(model, newdata = predictiction_data)

#lets plot some regression and corelations 
wait_select<-subset(Wait_time, select = c(wait,AheadCount))
pairs.panels(wait_select, col="red")
                                                              
wait_select<-subset(Wait_time, select = c(wait,DelayedInLine))                                            
          


     
     
