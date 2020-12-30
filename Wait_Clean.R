waitdata<-read.csv("Wait_Time.csv")
#create a column for appointments by days of week 
library(lubridate)

waitdata$parsedappt<- parse_date_time(waitdata$x_ScheduledDTTM, 'y-m-d H:M:S')

waitdata$calendar<- date(waitdata$parsedappt)

#create a  table to start the data frame 
calendarsum<- table(waitdata$calendar)
segmenteddata<- as.data.frame (calendarsum)

colnames(segmenteddata)<- c('date', 'sumofappts')
colnames(waitdata)

#add add wait times per day 

segmenteddata$dailywaitsum<-tapply(waitdata$ï..Wait, waitdata$calendar, FUN = sum)

#number of add on per day 

segmenteddata$num_add_ons_today <- tapply(waitdata$NumAddOnsToday, waitdata$calendar, FUN= sum)
segmenteddata$number_completed_today<-tapply(waitdata$NumCompletedToday, waitdata$calendar, FUN= sum)
segmenteddata$day_of_week<-tapply(waitdata$DayOfWeek, waitdata$calendar, FUN = mean)
segmenteddata$outpatientwaitcount<-tapply(waitdata$OutpatientWaitingCount, waitdata$calendar, FUN= sum)
segmenteddata$in_progress_size<-tapply(waitdata$InProgressSize, waitdata$calendar, FUN= sum)
segmenteddata$ahead_count<-tapply(waitdata$AheadCount, waitdata$calendar, FUN= sum)
segmenteddata$num_scanners_used_today<-tapply(waitdata$NumScannersUsedToday, waitdata$calendar, FUN= sum)
segmenteddata$with_and_without_contrast_wait<-tapply(waitdata$WithAndWithoutContrastCountWaiting, waitdata$calendar, FUN= sum)
segmenteddata$with_or_without_contrast_progress<-tapply(waitdata$WithAndWithoutContrastCountInProgress, waitdata$calendar, FUN= sum)
segmenteddata$with_contrast_count_waiting<-tapply(waitdata$WithContrastCountWaiting, waitdata$calendar, FUN= sum)
segmenteddata$delay_count<-tapply(waitdata$DelayCount, waitdata$calendar, FUN= sum)
segmenteddata$delayed_in_line<-tapply(waitdata$DelayedInLine, waitdata$calendar, FUN= sum)
segmenteddata$avg_delay_for_day<-tapply(waitdata$AvgDelayForDay, waitdata$calendar, FUN= sum)
segmenteddata$thoraic_count<-tapply(waitdata$ThoracicCount, waitdata$calendar, FUN= sum)
segmenteddata$msk_count<-tapply(waitdata$MSKCount, waitdata$calendar, FUN= sum)
segmenteddata$cardiac_count<-tapply(waitdata$CardiacCount, waitdata$calendar, FUN= sum)
segmenteddata$vascular_count<-tapply(waitdata$VascularCount, waitdata$calendar, FUN= sum)
segmenteddata$numbercompleted<-tapply(waitdata$AbdominalCount, waitdata$calendar, FUN= sum)
segmenteddata$numbercompleted<-tapply(waitdata$NeuroCount, waitdata$calendar, FUN= sum)

#Building models 
colnames(segmenteddata)

model1<- lm(dailywaitsum ~ sumofappts+num_add_ons_today+number_completed_today
            +day_of_week+outpatientwaitcount+in_progress_size+ahead_count+num_scanners_used_today
            +with_and_without_contrast_wait+with_or_without_contrast_progress+with_contrast_count_waiting
            +delay_count+delayed_in_line+avg_delay_for_day+thoraic_count+msk_count
            +cardiac_count+vascular_count+numbercompleted, data=segmenteddata)

summary(model1)
plot(model1)

anova(model1)
influence(model1)
vcov(model1)

#comparing models model consist of 11 significant p values
model2 <- lm(dailywaitsum ~ sumofappts+num_add_ons_today+number_completed_today
             +day_of_week+outpatientwaitcount+in_progress_size+ahead_count
             +delay_count+delayed_in_line+avg_delay_for_day, data=segmenteddata)

summary(model2)
plot(model2)

#plots for model 2, <return to see the plots > can we make the plots look more pretty?
plot(dailywaitsum ~ sumofappts+num_add_ons_today+number_completed_today
     +day_of_week+outpatientwaitcount+in_progress_size+ahead_count
     +delay_count+delayed_in_line+avg_delay_for_day, data=segmenteddata)

anova(model1, model2)

#reorder variables so those with the highest correlation is closer to the diagonal

#relative importance
library(relaimpo)
calc.relimp(model2,type=c("lmg","last","first","pratt"),
            rela=TRUE)
