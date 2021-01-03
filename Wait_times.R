Wait_time<-read.csv("Wait_Time.csv")
colnames(Wait_time)
head(Wait_time)
summary(Wait_time)


#rename "wait"
names(Wait_time)[1]<-"wait"

#relevant columns, clean up 
Wait_relevant<-(Wait_time[,c(1,5:32)])
colnames(Wait_relevant)

#Transform date format

set.seed(5)
ran<- sample(1:nrow(Wait_relevant),.75*nrow(Wait_relevant))

waitdata_train<- Wait_relevant[ran,c(1:29)]
waitdata_test<- Wait_relevant[-ran,c(1:29)]

#create Exploratory model Train
model1_train<-lm(wait~SumHowEarlyWaiting+AvgHowEarlyWaiting+LineCount0Strict+SumWaits
           +LineCount0+LineCount1+LineCount2+LineCount3+LineCount4+SchFlowCount2
           +SchFlowCount4+FutFlowCount2+FutFlowCount4+DelayCount+DelayCountLastHour
           +mintime+maxtime+AheadCount+ThoracicCount+PediatricCount+NeuroCount+AbdominalCount
           +MSKCount+NumScannersUsedToday,data = waitdata_train)
summary(model1_train)

plot(model1_train)

anova(model1_train)
influence(model1_train)
vcov(model1_train)

#create Exploratory model Test
model1_test<-lm(wait~SumHowEarlyWaiting+AvgHowEarlyWaiting+LineCount0Strict+SumWaits
           +LineCount0+LineCount1+LineCount2+LineCount3+LineCount4+SchFlowCount2
           +SchFlowCount4+FutFlowCount2+FutFlowCount4+DelayCount+DelayCountLastHour
           +mintime+maxtime+AheadCount+ThoracicCount+PediatricCount+NeuroCount+AbdominalCount
           +MSKCount+NumScannersUsedToday,data = waitdata_test)
summary(model1_test)

plot(model1_test)

anova(model1_test)
influence(model1_test)
vcov(model1_test)

#Create a Secondary Model based on the best correlations 
model2_train<-lm(wait~SumHowEarlyWaiting+AvgHowEarlyWaiting+LineCount0Strict+SumWaits
           +LineCount0+SchFlowCount4+FutFlowCount2+FutFlowCount4+DelayCount+DelayCountLastHour
           +maxtime+AheadCount+ThoracicCount+NumScannersUsedToday,data = waitdata_train)
summary(model2_train)

plot(model2_train)


anova(model2_train)
influence(model2_train)
vcov(model2_train)

model2_test<-lm(wait~SumHowEarlyWaiting+AvgHowEarlyWaiting+LineCount0Strict+SumWaits
                 +LineCount0+SchFlowCount4+FutFlowCount2+FutFlowCount4+DelayCount+DelayCountLastHour
                 +maxtime+AheadCount+ThoracicCount+NumScannersUsedToday,data = waitdata_test)
summary(model2_test)

plot(model2_test)


anova(model2_test)
influence(model2_test)
vcov(model2_test)


