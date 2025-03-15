###GOAL: -Create a classification model that can predict whether a transaction is legitimate or fraudulent based on the features in the data.
### - Minimize false negative to protect the financial system.

#Library needed for this model 
library(randomForest) #model 
library(caret) 
library(pROC) #AUC ROC 



#Split the data into 70/30 : Maintain  the class ratio 
#Create training and testing sets: We split the data into two parts: training (70% of the data) and testing (30% of the data).
#The training set is used to train the model, and the testing set is used to evaluate the model's performance. This helps assess the model's generalization ability.
set.seed(123)
train_index <- createDataPartition(df$Class, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]
train_data
test_data

#This part is to ensure Class is Factor and not Int 
train_data$Class <- as.factor(train_data$Class)
test_data$Class <- as.factor(test_data$Class)



#From the analysis and visualisation tab, it can be seen that fraud transactions are very few compared to valid transactions (ratio is 1:0.001730475)
#PROBLEM: Model can therefore reach 99ish % accuracy, which completely useless in reality 
#SOLUTION: Randomforest   

model_rf <- randomForest(Class ~ ., data = train_data, 
                         classwt = c("0" = 1, "1" = 10))
#Since fraudulent transactions are more important to detect, we give higher weight to the fraudulent class during training.
#This makes it more sensitive to fraud 

predictions_rf <- predict(model_rf, newdata = test_data)
#Generate predictions based on model 
predictions_rf <- factor(predictions_rf, levels = c("0", "1"))
#Necessary to perform confusionMatrix below 


confusion_rf <- confusionMatrix(predictions_rf, test_data$Class)
#extremely important to provide a breakdown of :
#True Positives (TP): Fraud correctly identified as fraud.
#True Negatives (TN): Legitimate transactions correctly identified as legitimate.
#False Positives (FP): Legitimate transactions incorrectly identified as fraud.
#False Negatives (FN): Fraudulent transactions incorrectly identified as legitimate.
#Accuracy, Sensitivity (Recall), Specificity, Precision.

#AUC ROC for Random Forest.
#Compare the predicted values with the actual values and calculates the ROC curve,
# which shows how well the model can separate fraud from legitimate transactions.
roc_rf <- roc(test_data$Class, as.numeric(predictions_rf))
print(roc_rf)
plot(roc_rf, main = "ROC Curve for Fraud Detection", col = "green", lwd = 2)
text(0.6, 0.4, labels = paste("AUC =", round(roc_rf$auc, 3)), col = "red", cex = 1.5)