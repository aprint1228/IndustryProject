# IndustryProject
Wait_time<-read.csv("Wait_Time.csv")
colnames(Wait_time)
head(Wait_time)

#extract departments from the data set
Departments<-data.frame(Wait_time$ThoracicCount,Wait_time$PediatricCount,Wait_time$NeuroCount,
Wait_time$AbdominalCount,Wait_time$VascularCount,Wait_time$CardiacCount
,Wait_time$MSKCount,Wait_time$SumWaits)


#show department with most wait time. 
summary(Departments)
# According to the summary Thoracic is the department with most wait time 
#followed by Abdominal department

hist(Departments$Wait_time.ThoracicCount)
hist(Departments$Wait_time.AbdominalCount)
hist(Wait_time$SumWaits)

#Department vs scans used
Departments_scans<-data.frame(Departments,Wait_time$NumScannersUsedToday)
summary(Departments_scans)
plot(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.SumWaits)

#Is there a relationship between scan and wait time?

plot(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.SumWaits)
model<-lm(Departments_scans$Wait_time.SumWaits~Departments_scans$Wait_time.NumScannersUsedToday, data = Departments_scans)
abline(model, col='red')
coefficients(model)
summary(model)
cor(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.SumWaits)

#how about the two highest wait time departments? (Thoracic and abdominal)

plot(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.ThoracicCount  )
model_Thoracic<-lm(Departments_scans$Wait_time.ThoracicCount~Departments_scans$Wait_time.NumScannersUsedToday, data = Departments_scans)
abline(model_Thoracic, col='red')
coefficients(model_Thoracic)
summary(model_Thoracic)
cor(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.SumWaits)

#abdominal
plot(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.AbdominalCount  )
model_abdominal<-lm(Departments_scans$Wait_time.AbdominalCount~Departments_scans$Wait_time.NumScannersUsedToday, data = Departments_scans)
abline(model_abdominal, col='red')
coefficients(model_abdominal)
summary(model_abdominal)
cor(Departments_scans$Wait_time.NumScannersUsedToday,Departments_scans$Wait_time.SumWaits)
