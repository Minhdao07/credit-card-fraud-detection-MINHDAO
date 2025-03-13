df <- read.csv("/Users/hoangminh/Downloads/creditcard.csv")
#Install and call out required libraries 
library(ggplot2)
library(tidyverse)
library(pROC)
#Simply get first few ideas of the data
View(df)
str(df)
summary(df)


#Check for mising data 
sum(is.na(df))
#Seems like there are no missing data. The data is ready to be discovered

#Since V1 -> V28 has already been standardized, column Amount and Time should do the same
df$Amount <- scale(df$Amount)
df$Time <- scale(df$Time)


#Fraud-to-Legit Ratio
#GOAL: Determine the fraudulent rate
fraud_transaction <- df %>% filter(Class == 1)
head(fraud_transaction)
Num_fraud_transaction <- sum(df$Class == 1)

legit_transaction <- df %>% filter(Class == 0)
head(legit_transaction)
Num_legit_transaction <- sum(df$Class == 0)

fraud_legit_ratio <- Num_fraud_transaction / Num_legit_transaction
fraud_legit_ratio

#SOME IMPORTANT NUMBERS:
#Fraudulent transactions: 492  
#Legitimate transactions: 284315  
#Fraud-to-legit transaction ratio: 0.001730475



#Transaction amount distribution 
#GOAL: 
#Since the amount of money from fraudulent transactions are too small compare to legit ones, log transformation would be better to make it more visible 
ggplot(df, aes(x = log(Amount), fill = factor(Class))) +  
  geom_histogram(bins = 50, alpha = 0.7) +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "Log-Transformed Transaction Amount Distribution",
       x = "Log(Transaction Amount)", y = "Count") +
  theme_minimal()

#Time-Based Fraud Analysis 
#GOAL : Determine if fraud occurs more at a particular time of day.

ggplot(fraud_transaction, aes(x = Time)) +
  geom_histogram(bins = 50, fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribution of Fraudulent Transactions Over Time",
       x = "Time (seconds since first transaction)", y = "Number of fraudulant in the given time") +
  theme_minimal()


#ROC Curve using logistic regression model 
#GOAL : Determine how the model perform  

#We need the logistic regression to calculate the probability of a transaction is fraudulent 
model <- glm(Class ~ ., data = df, family = binomial())
model

#predicted value base on the above model 
df$predicted <- predict(model, df, type = "response")

# Precision-Recall Curve
pr <- roc(df$Class, df$predicted, plot = TRUE, print.auc = TRUE)


# Boxplot distribution between Amount and Class (Fraud vs Legit)
#GOAL: Transaction amounts can be an important factor in distinguishing legitimate transactions from fraudulent ones. Analyzing transaction amounts can help you spot fraudulent transaction patterns.
ggplot(df, aes(x = factor(Class), y = Amount, fill = factor(Class))) +
  geom_boxplot() +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "Distribution of Transaction Amounts by Fraud (Class)",
       x = "Class (0 = Legit, 1 = Fraud)", y = "Transaction Amount") +
  theme_minimal()

#Scatter Plot between V1 and V2
#GOAL: The scatter plot between V1 and V2 aims to visualize the separation between fraudulent and legitimate transactions in PCA space.
ggplot(df, aes(x = df$V1, y = df$V2, color = factor(Class))) +
  geom_point(alpha = 0.6) +
  labs(x = "V1", y = "V2", title = "Scatter Plot between V1 and V2 by Transaction Class") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

# In case one needs to determine a relationship between any V in the table, this function below will do the trick.
# Filter columns "V" only:
# Create scatter plot function between any two columns (V1, V2, etc.) in the PCA dataset
Create_scatter_plot_any_2Vs <- function(df_only_Vs, X, Y) {

  # Vẽ scatter plot giữa V1 và V2
  ggplot(df, aes(x = X, y = Y, color = factor(Class))) +
    geom_point(alpha = 0.6) +
    labs(x = X, y = Y, title = paste("Scatter Plot between", X, "and", Y, "by transaction class")) +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
}

# Đảm bảo bạn có df_only_Vs chỉ chứa các cột V1 đến V28 (không có Time và Amount)
df_only_Vs <- df %>% select(starts_with("V"))

Create_scatter_plot_any_2Vs(df_only_Vs, df_only_Vs$V3, df_only_Vs$V4)





