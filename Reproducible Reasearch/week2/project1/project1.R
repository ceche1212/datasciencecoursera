library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
#read the table

tabla<-read.csv("activity.csv")
daily_steps = tapply(tabla$steps,tabla$date,sum,na.rm=FALSE)
daily_steps = data.frame(daily_steps)

#calculate values by day

tabla$date<-as.Date(tabla$date)
daily_steps[,"dates"]<-c(unique(tabla$date))
plot1<-ggplot(daily_steps,aes(x= daily_steps))
plot1 + geom_histogram(fill="steelblue",binwidth = 1500) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")

#calculate the mean and median

pregunta2<-c(mean(daily_steps[,1],na.rm = TRUE),median(daily_steps[,1],na.rm=TRUE))
pregunta2

#data by intervals
steps_by_interval <- tapply(tabla$steps,tabla$interval,mean,na.rm=TRUE)
steps_by_interval = data.frame(steps_by_interval)
steps_by_interval[,"intervals"]=c(unique(tabla$interval))
plot2<-ggplot(steps_by_interval,aes(x=intervals,y=steps_by_interval))
plot2+geom_line(color="red",size=2)+labs(title = "Avg Steps by interval",y="steps")

#maximo intervalo

filter(steps_by_interval,steps_by_interval==max(steps_by_interval))

#number of missing values in steps and replacing for the median of steps

summary(tabla$steps)

mediana_pasos<-median(tabla$steps,na.rm = TRUE)

copia = copy(tabla)

copia[is.na(copia$steps),"steps"]<-c(mediana_pasos)

#new histogram with missing values replaced by median

daily_steps_copia = tapply(copia$steps,copia$date,sum,na.rm=FALSE)
daily_steps_copia = data.frame(daily_steps_copia)
daily_steps_copia[,"dates"]<-c(unique(copia$date))
colnames(daily_steps_copia)<-c("steps","dates")
plot3<-ggplot(daily_steps_copia,aes(x=steps))
plot3+geom_histogram(fill="red",binwidth = 1500)+labs(title = "Daily Steps", x = "Steps", y = "Frequency")

#difference in numbers fater NA values replaced
summary(daily_steps_copia)

#panel plot for week or weekend day

copia[,"weekday"]<-weekdays(copia$date)

patron_sem<-("Monday|Tuesday|Wednesday|Thursday|Friday")
patron_finde<-("Saturday|Sunday")
sem<-grepl(patron_sem,copia$weekday,ignore.case = TRUE)
findesemana<-grepl(patron_finde,copia$weekday,ignore.case = TRUE)
copia[sem,"sem_o_fin"]<-c("weekday")
copia[findesemana,"sem_o_fin"]<-c("weekend")

weekday<-filter(copia,sem_o_fin=="weekday")
weekday_by_interval<-as.data.frame(tapply(weekday$steps,weekday$interval,mean,na.rm=TRUE))
weekday_by_interval[,"intervals"]=c(unique(weekday$interval))
colnames(weekday_by_interval)<-c("Average_Steps","interval")

weekend<-filter(copia,sem_o_fin=="weekend")
weekend_by_interval<-as.data.frame(tapply(weekend$steps,weekend$interval,mean,na.rm=TRUE))
weekend_by_interval[,"intervals"]=c(unique(weekend$interval))
colnames(weekend_by_interval)<-c("Average_Steps","interval")

plot5<-ggplot(weekday_by_interval,aes(x=interval,y=Average_Steps))
plot5<-plot5+geom_line(color="steelblue")+labs(title = "Weekday")
plot6<-ggplot(weekend_by_interval,aes(x=interval,y=Average_Steps))
plot6<-plot6+geom_line(color="red")+labs(title = "Weekend")

grid.arrange(plot5,plot6,nrow=1)


