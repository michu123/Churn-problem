rm(list=ls())
setwd('')

#library packages
#install.packages('pacman')
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


#missing value       =>    11/1: impute mode, median     
table(is.na(data))
colSums(is.na(data))


#remove customerID 
data$customerID<-NULL

#data transformation 1: e.g. No internet service-->NO
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

data$PhoneService<- as.factor(mapvalues(data$PhoneService, 
                                           from=c("No Phone service"),
                                           to=c("No")))
#data transformation 2: tenure
min(data$tenure); max(data$tenure)
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
data$tenure_group <- sapply(data$tenure,group_tenure)
data$tenure_group <- as.factor(data$tenure_group)

data$tenure <- NULL


#dummy variable: PaymentMethod. Contract. InternetService 
for (dummy=c)
if(!require(dummy, quietly = TRUE)) install.packages('dummy', quiet = TRUE) ; require(dummy, quietly = TRUE)


cats1 <- categories(data[,'PaymentMethod', drop = FALSE])
cats2 <- categories(data[,'Contract', drop = FALSE])

PaymentMethod1 <- dummy::dummy(x = data[,'PaymentMethod', drop = FALSE],object = cats1, int = FALSE)
Contract1 <- dummy::dummy(x = data[,'Contract', drop = FALSE],object = cats2, int = FALSE)

sapply(PaymentMethod1, table)/nrow(PaymentMethod1)
sapply(Contract1, table)/nrow(Contract1)

data$PaymentMethod <- NULL
data <- cbind(PaymentMethod1,data)
head (data)

data$Contract <- NULL
data <- cbind(Contract1,data)

data$InternetService <- as.numeric(as.factor(mapvalues(data$InternetService , 
                                        from=c("No","DSL","Fiber optic"),
                                        to=c("0","1","2"))))-1
table(data$InternetService)



#correlation
library(reshape)
b=sapply(data, is.numeric)
ndata<-data[, sapply(data, is.numeric)]
a <- cor(ndata)
#a[a == 1] <- NA #drop perfect
#a[abs(a) < 0.5] <- NA # drop less than abs(0.5)
a <- na.omit(melt(a)) 
a[order(abs(a$value),decreasing = TRUE),] 

# remove Variables: high correlated: MonthlyCharges         TotalCharges 
data$TotalCharges <- NULL




#plot(ndata)
p1 <- ggplot(data, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(data, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(data, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(data, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)
p5 <- ggplot(data, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(data, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(data, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(data, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)




#Train, validation, test data
ind <-sample(x =1:nrow(data), size = nrow(data),replace = FALSE)
trainind <- ind[1:round(length(ind)*.70)]
valind <- ind[(round(length(ind)*.70)+1):round(length(ind)*.85)]
testind <- ind[round(length(ind)*.85+1):length(ind)]


#Test whether there are no intersects
intersect(trainind, valind)
intersect(valind, testind)
intersect(trainind,testind)

#Create the sets and separate the response
train <- data[trainind,]
y_train <- train$Churn
train$Churn <- NULL

test <- data[testind,]
y_test <- test$Churn
test$Churn <- NULL

val <- data[valind,]
y_val <- val$Churn
val$Churn <- NULL

trainBIG <- rbind(train,val)

y_trainBIG <- as.factor(c(as.character(y_train),as.character(y_val)))

table(y_train); table(y_val); table(y_test); table(y_trainBIG)

######### Logistic regression
LR <- glm(formula = y_trainBIG~., data = trainBIG, family = binomial("logit"))
LR
