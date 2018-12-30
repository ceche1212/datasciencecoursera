library(dplyr)
library(ggplot2)
library(caret)
library(hflights)
library(corrplot)
library(lattice)
library(rattle)

data("hflights")
tabla<-hflights



datavuelos<-select(tabla,c(2:5),c(7),c(12:16),c(19),c(21))
str(datavuelos)
summary(datavuelos)

#Removing NA's'from the dataset substitution for the median

#Departure time

mediandeptime<-median(datavuelos$DepTime,na.rm = TRUE)
mediandeptime

datavuelos[is.na(datavuelos$DepTime),4]<-mediandeptime

# ArrDelay

medianArrDelay<-median(datavuelos$ArrDelay,na.rm = TRUE)
datavuelos[is.na(datavuelos$ArrDelay),6]<-medianArrDelay

#DepDelay

medianDepDelay<--median(datavuelos$DepDelay,na.rm = TRUE)
datavuelos[is.na(datavuelos$DepDelay),7]<-medianDepDelay

#creating a factor variable

datavuelos[datavuelos$ArrDelay>0,"Status"]="DELAYED"
datavuelos[datavuelos$ArrDelay<=0,"Status"]="ON TIME"

datavuelos$Status<-as.factor(datavuelos$Status)

#datapartition

intrain<-createDataPartition(y=datavuelos$Status,p=0.7,list = FALSE)
training<-datavuelos[intrain,]
testing<-datavuelos[-intrain,]

#model with trees

modelfit1<-train(Status ~ Month + DayofMonth + DayOfWeek + DepTime + UniqueCarrier + Origin + Dest, data = training, method = "rpart")
predictionsmodel1<-predict(modelfit1,newdata = testing)
confusionMatrix(predictionsmodel1,as.factor(testing$Status))

fancyRpartPlot(modelfit1$finalModel)



modelfit2<-train(Status ~ Month + DayofMonth + DayOfWeek + DepTime + UniqueCarrier + Origin + Dest, data = training, method = "glm")
predictionsmodel2<-predict(modelfit2,newdata = testing)
confusionMatrix(predictionsmodel2,as.factor(testing$Status))

