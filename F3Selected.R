Wait_time<-read.csv("F3Selected.csv")
colnames(Wait_time)
head(Wait_time)
summary(Wait_time)


#models with wait as independent variable 
model1<-lm(Wait~AvgDelayForDay+NumCompletedToday+
          NumAddOnsToday+mintime+maxtime+DayOfWeek+Month+
            DayOfYear+AvgAgePeopleWaiting+OutpatientWaitingCount
          +AheadCount+InProgressSize+MalesWaitingCount+
            WithAndWithoutContrastCountWaiting+WithContrastCountWaiting, data = Wait_time)
summary(model1)
confint(model1, conf.level=0.95)
plot(model1)

# Next is testing and predicting models 



