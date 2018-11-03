# 1.Logistic Regression
print(summary(LR))

# Feature Analysis:
# The top three most-relevant features include Contract, tenure_group and PaperlessBilling.
anova(LR, test="Chisq")

# Logistic Accuracy
test$Churn <- as.character(test$Churn)
test$Churn[test$Churn=="No"] <- "0"
testing$Churn[test$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Logistic Confusion Matrix
print("Confusion Matrix for Logistic Regression"); 
table(test$Churn, fitted.results > 0.5)

# AUC:
# make a prediction
predLR <- as.numeric(predict(LR,test, type='response')) 
if(!require(AUC, quietly = TRUE)) install.packages('AUC', quiet = TRUE) ; require(AUC, quietly = TRUE)
(auc_lr <- AUC::auc(roc(predLR, y_test))) 

# ROC:
if(!require(pROC, quietly = TRUE)) install.packages('pROC', quiet = TRUE) ; require(pROC, quietly = TRUE)
data(test$Churn)
# Build a ROC object and compute the AUC, draw ROC, print AUC and the best THRESHOLDS
roc(test$Churn, predLR, plot=TRUE, print.thres=TRUE, print.auc=TRUE)
# Build several ROCs in one graph
roc1 <- roc(test$Churn, predLR)
roc2 <- roc(test$Churn, predLR)
plot(roc1, col="blue")
plot.roc(roc2, add=TRUE, col="red")


# 2.Decision Tree
# Decision Tree Confusion Matrix
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

# Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

# 3.Naive Bayes
# AUC
if (!require('AUC')) { 
  install.packages('AUC',
                   repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require('AUC') 
}

(auc_nb <- AUC::auc(roc(predNB,y_test)))
plot(roc(predNB, y_test))

# REFERENCES:https://www.kaggle.com/liyingiris90/telco-customer-churn-prediction

###########################################################################################################
