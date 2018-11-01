rm(list=ls())
setwd('C:/Users/R/Google ¶³ºÝµwºÐ/Postschool/Course/Compulsory/Predictive Analytics and Modelling of Data/Project')

#library packages
install.packages('pacman')
require('pacman')
p_load("plyr", "corrplot", "ggplot2", "gridExtra", "ggthemes","data.table", "caret", "MASS", "randomForest", "party","plyr")



#import data
data<-read.csv('Edinburgh_train_sample.csv')
classes<-sapply(data,class)
data<-read.csv('Edinburgh_train_sample.csv',header=TRUE,sep=',',colClasses=classes)
str(data)
head(data)
names(data)
dim(data)
# summary statisics
summary(data)
lapply(data, unique)

#find 11 ppl no phone service ==> missing value
table(data$PhoneService)

#missing value         -->remove it 
table(is.na(data))
colSums(is.na(data))
data <- data[complete.cases(data), ]

#remove customerID &PhoneService(only YES)
data$customerID<-NULL
data$PhoneService<-NULL
data$InternetService<-NULL
#data transformation  e.g. No internet service-->NO
data$MultipleLines <- as.factor(mapvalues(data$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
data$OnlineSecurity <- as.factor(mapvalues(data$OnlineSecurity, 
                                           from=c("No internet service"),
                                           to=c("No")))
data$OnlineBackup <- as.factor(mapvalues(data$OnlineBackup, 
                                           from=c("No internet service"),
                                           to=c("No")))
data$DeviceProtection <- as.factor(mapvalues(data$DeviceProtection, 
                                           from=c("No internet service"),
                                           to=c("No")))
data$TechSupport<- as.factor(mapvalues(data$TechSupport, 
                                           from=c("No internet service"),
                                           to=c("No")))
data$StreamingTV<- as.factor(mapvalues(data$StreamingTV, 
                                       from=c("No internet service"),
                                       to=c("No")))
data$StreamingMovies<- as.factor(mapvalues(data$StreamingMovies, 
                                       from=c("No internet service"),
                                       to=c("No")))


#correlation
library(reshape)
b=sapply(data, is.numeric)
ndata<-data[, sapply(data, is.numeric)]
a <- cor(ndata)
a[a == 1] <- NA #drop perfect
a[abs(a) < 0.5] <- NA # drop less than abs(0.5)
a <- na.omit(melt(a)) 
a[order(abs(a$value),decreasing = TRUE),] 

#plot(ndata)
cor(ndata)


#Logistic Regression: # of training data= 0.5
intrain<- createDataPartition(data$Churn,p=0.5,list=FALSE)
set.seed(100)
training<- data[intrain,]
testing<- data[-intrain,]
dim(training); dim(testing)

lapply(training, unique)


M1 <- glm(Churn ~ .,family=binomial(link='logit'),data=training)
summary(M1)


