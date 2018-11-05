#example from https://jtsulliv.github.io/churn-prediction/

rm(list=ls())

#install  .packages('rlang',tidyverse', 'miscset')
#library packages
library(tidyverse); library(miscset);  library(corrplot); library(ggplot2); library(gridExtra); 

#import data
setwd('C:/Users/michu/Desktop/EdU study/Predictive Analytics & Modelling of Data/Term Project')
data<-read.csv('train_sample.csv')
dim(data) #dimensions of the data
names(data) #variables names from the list
str(data) #data types
head(data,2) #1st 2 rows from the list

#summary statisics
summary(data)
lapply(data, unique) #except continuous variables, only customerID  has all unique values, which should be deleted as no predicting power can be brought.  
#Other categorical variable contain at least 2 unique values that might be used in further training. 
#Though there are some variables that contain values of "No internet service" | "No internet service", which can be trimmed to --> "No".
#Tenure varaible values explain duration of being with company for customers and it will be grouped in to buckets later. 
sapply(data, function(x) sum(is.na(x))) #to check missing values in the dataset. It has 21 missing rows in various columns and it is around 20% from the total rows. It significant amount of this data, therefore we decided to impute them.

#Method1: impute Median or mode
library(imputeMissings)
data_imp <- impute(data = data, flag = FALSE)
sapply(data, function(x) sum(is.na(x))) #recheck of missing values in the list
data<-data_imp

# #Method2: random forest   => 1. can not impute factors 2. normally done on a training set 
# colSums(is.na(data))
# selection <- c('TotalIntlCalls', 'TotalDayCalls', 'TotalEveMinutes', 'TotalIntlMinutes','TotalDayMinutes','TotalDayMinutes','TotalEveCalls','TotalNightCalls','TotalNightMinutes')
# subs <- data[,selection]
# values <- compute(subs, method = 'randomForest')
# subs_imp <- impute(data = subs, object = values)
# 
# #Method3 remove the two columns
# data$VoiceMailPlan<-NULL
# data$InternationalPlan<-NULL

#data transformation 1: e.g. No internet service | No phone service-->NO
library(plyr)
transform1 <- c("No phone service","No internet service")
transform2 <- which(Vectorize(function(x) x %in% transform1)(data), arr.ind=TRUE) #find variables with No internet service|No phone service
summary(transform2) #to find out range of rows that contain "No phone service","No internet service"
cols_trnfrm <- c(8:15) #columns from 8 to 15 contain values that need to be replaced. 
for(i in 1:ncol(data[,cols_trnfrm])) {
  data[,cols_trnfrm][,i] <- as.factor(mapvalues
                                      (data[,cols_trnfrm][,i], from =c("No phone service","No internet service"),to=c("No","No")))
} 
lapply(data, unique) #recheck transformation of values

#data transformation 2: place group tenure into buckets 
#min(data$tenure); max(data$tenure) #minimum tenure is 1 & maximum is 72 & group them in 5 buckets
#group_tenure <- function(tenure){
#  if (tenure >= 0 & tenure <= 12){
#    return('0-12 Month')
#  }else if(tenure > 12 & tenure <= 24){
#    return('12-24 Month')
#  }else if (tenure > 24 & tenure <= 48){
#    return('24-48 Month')
#  }else if (tenure > 48 & tenure <=60){
#    return('48-60 Month')
#  }else if (tenure > 60){
#    return('> 60 Month')
#  }} 

#data$tenure_group <- sapply(data$tenure,group_tenure) #create tenure_group variable in the dataset
#data$tenure_group <- as.factor(data$tenure_group) #convert tenure_group variable as a factor in data #not used in current logistic model

#change the values in column "SeniorCitizen" from 0 or 1 to "No" or "Yes".
data$SeniorCitizen <- as.factor(mapvalues(data$SeniorCitizen,
                                          from=c("0","1"),
                                          to=c("No", "Yes")))

#Exploratory data analysis and feature selection after some data cleaning
#for analysis, let's visualize & review churn relationship in:
  #1)  men vs women?
  #2)  churn likelihood for senior citizens
  #3)  churn likelihood for customers with partners
  #4)  churn likelihood for customer with depedents

library(dplyr,warn.conflicts = F); library(rlang); library(ggplot2)
plotGender <- ggplot(data) + 
geom_bar(aes(x = gender, fill = Churn), position = "dodge") #churners split almost evenly for both genders, will go for deeper review

data %>% 
group_by(gender,Churn) %>% 
summarise(n=n()) #plot's summary table

plotSeniorCit <- ggplot(data) + 
geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge") #numbers show that from 84% non-senior customers have 22% churning rate and 16% of senior citizens have 25% churning rate.

data %>% 
group_by(SeniorCitizen) %>% 
summarise(n = n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table split yes & no in Senior Citizens
 
data %>% 
group_by(SeniorCitizen, Churn) %>% 
summarise(n = n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Senior Citizens

plotPartner <- ggplot(data) +
geom_bar(aes(x=Partner, fill = Churn), position = "dodge") #partner category is split almost evenly and partner.yes have 30% churn, while others only 14%

data %>% 
group_by(Partner) %>% 
summarise(n = n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table split yes & no in Partner

data %>% 
group_by(Partner, Churn) %>% 
summarise(n = n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Partners

plotDepend <- ggplot(data) + 
geom_bar(aes_string(x="Dependents", fill="Churn"), position = "dodge") #majority of 67% of customers don't have dependents and they have churning rate of 27% churn.

data %>% group_by(Dependents) %>% 
summarise(n = n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table split yes & no in Dependents

data %>% group_by(Dependents, Churn) %>% 
summarise(n=n()) %>% 
mutate(freq = n / sum(n)) #plot's summary table in churning rate in split of Dependents

#To identify outliers, deeper analysis total charges amounts for customer segments who we noticed high churning potenital.
boxplotSeniorCit <- ggplot(data, aes(x = SeniorCitizen, y = TotalCharges)) + 
geom_boxplot() #on average Senior Citizens spend more

boxplotPartner <- ggplot(data, aes(x = Partner, y = TotalCharges)) + 
geom_boxplot() #as per stats Partner.yes spend more 

boxplotDepend <- ggplot(data, aes(x = Dependents, y = TotalCharges)) + 
geom_boxplot() #Dependents.no spend more on average

data %>% 
select(SeniorCitizen, Churn, TotalCharges, tenure) %>% 
filter(SeniorCitizen == "Yes", Churn == "Yes") %>% 
summarize(n = n(),
total = sum(TotalCharges),
avg_tenure = sum(tenure)/n) #stats high-potential churners in senior citzens.yes segment

data %>% 
select(Partner, Churn, TotalCharges, tenure) %>% 
filter(Partner == "No", Churn == "Yes") %>% 
summarise(n = n(),
total = sum(TotalCharges),
avg_tenure = sum(tenure)/n) #stats high-potential churners in partner.no segment

data %>% 
select(Dependents, Churn, TotalCharges, tenure) %>% 
filter(Dependents == "No", Churn == "Yes") %>% 
summarise(n = n(),
total = sum(TotalCharges),
avg_tenure = sum(tenure)/n) ##stats high-potential churners in dependents.no segment
###As preliminary outcome, it is should be highlighted that from 3 customer segments, dependents.no have highest value that result in churners. So, in further visual analysis we will focus on them in more depth. 

dependents <- data %>% filter(Dependents == "No") #filter for plots variables selection

ggplotGrid(ncol=2,
lapply(c("PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup",
"DeviceProtection"),
function(col){
ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
})) #visualiztion of customers from segment dependents.no for various variables in dataset 

ggplotGrid(ncol=2,
lapply(c("TechSupport","StreamingTV","StreamingMovies","Contract",
"PaperlessBilling"),
function(col){
ggplot(dependents,aes_string(col)) + geom_bar(aes(fill=Churn),position="dodge")
})) #visualiztion of customers from segment dependents.no for various variables in dataset 

ggplot(dependents) +
geom_bar(aes(x=PaymentMethod,fill=Churn), position = "dodge") #visualiztion of customers from segment dependents.no for various variables in dataset 

###Outcome from analysis:
 #1)  A lot of customers with phone service churned. 
 #2)  Vast majority of customer with fiber optic internet churned much more than people with DSL or no internet at all.  
 #3)  People without online backup, device protection, and online security churn fairly frequently. 
 #4)  Similarly to online backup and security, those without device protection tended to churn more than those that subscribed ot the service.  Adding device protection to their plans may be a good way to prevent churn.
 #5)  Those without tech support tend to churn more frequently than those with tech support. 

#before training model, let's evaluate correlation & remove unnecessary variables
library(corrplot)
numeric.var <- sapply(data, is.numeric) #find numerical variables
corr.matrix <- cor(data[,numeric.var])  #calculate the correlation matrix
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number") #Monthly Charges & Total Charges are correlated. So one of them will be removed from the model. We remove Total Charges.
summary(corr.matrix)
#removed correlated items over 0.5
data$MonthlyCharges<- NULL
data$TotalDayCalls<-NULL
data$TotalEveCalls<-NULL
data$TotalIntlCalls<-NULL
data$TotalNightCalls<-NULL
#remove columns, which don't need for analysis 
data$customerID<-NULL
#data$tenure <- NULL #not deleting for logistic model

#Logistic regression model
#split the data into 70% training and 30% testing sets.
library(caret);library(stringi)
inTrain <- createDataPartition(y = data$Churn, p=0.70, list=FALSE)
train <- data[inTrain,]
test <- data[-inTrain,]

LogModel <- glm(Churn~., data=train, family=binomial) #fitting the model
head(LogModel,5)

library(stats); library(e1071)
ModelPrediction <- predict(LogModel, test, type="response") #making predictions
head(ModelPrediction,5)
# converting probabilities to classes; "Yes" or "No"
contrasts(data$Churn)  # Yes = 1, No = 0
glm.pred = rep("No", length(ModelPrediction))
glm.pred[ModelPrediction > 0.5] = "Yes"
glm.pred <-as.factor(glm.pred)
confusionMatrix(glm.pred, test$Churn, positive = "Yes") #per conf.matrix accuracy is 86%, let check 

library(ROCR) # need to create prediction object from ROCR
pr <- prediction(ModelPrediction, test$Churn)
# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#### Feature Selection to improve model
# summary of the model
summary(LogModel)
# feature importance 
sort(varImp(LogModel), decreasing = TRUE)
# fitting the model
fit <- glm(Churn~SeniorCitizen + tenure + MultipleLines + InternetService + StreamingTV + Contract + PaperlessBilling + PaymentMethod + TotalCharges
, data=train, 
family=binomial)
# making predictions
ModelPrediction <- predict(fit, test, type="response")
head(ModelPrediction,5)
# converting probabilities to classes; "Yes" or "No"
contrasts(data$Churn)  # Yes = 1, No = 0
glm.pred = rep("No", length(ModelPrediction))
glm.pred[ModelPrediction > 0.5] = "Yes"
glm.pred <- as.factor(glm.pred)
confusionMatrix(glm.pred, test$Churn, positive = "Yes")

library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(churn.probs, test$Churn)
# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
