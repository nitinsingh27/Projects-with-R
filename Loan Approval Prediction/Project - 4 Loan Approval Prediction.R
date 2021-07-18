setwd("C:/Users/enerc/OneDrive/Desktop/data science/sessions/r_training")
getwd()

library(dplyr)
library(Matrix)
library(ggplot2)
library(caTools)
library(e1071)
library(caret)

customer_loan <- read.csv("customer_loan.csv", stringsAsFactors = T)
loan <- customer_loan



# A) Data Preprocessing: 
# a. Have a glance at the structure of the dataset and find if there are any missing 
# values present 
# b. Calculate the debt-to-income ratio and add it as a new column named 'dti'
# c. Create a new variable named 'loan_decision_status', where the value would be 
# '0' if 'loan_decision_type' is equal to 'denied', else it would be '1'
# i. Convert this variable into a factor 
# d. Create a new data-set named 'customer_loan_refined', which would have 
# these column numbers from the original dataframe - (3,4,6,7,8,11,13,14) 
# e. Encode 'gender', 'marital_status', 'occupation', and 'loan_type' as factors and 
# then convert them into numeric 

str(customer_loan)
sum(is.na(customer_loan))

loan %>% mutate(dti = (debts/income)) -> loan

loan %>% mutate(loan_decision_status = ifelse( tolower(loan_decision_type)=="denied",0,1))  -> loan
loan$loan_decision_status <- as.factor(loan$loan_decision_status)

customer_loan_refined <- loan[,c(3,4,6,7,8,11,13,14)]

customer_loan_refined$gender   <- as.numeric(as.factor(customer_loan_refined$gender))
customer_loan_refined$marital_status <- as.numeric(as.factor(customer_loan_refined$marital_status))
customer_loan_refined$occupation<- as.numeric(as.factor(customer_loan_refined$occupation))
customer_loan_refined$loan_type<- as.numeric(as.factor(customer_loan_refined$loan_type))



# B) Model Building: 
# a. Divide the data into 'train' & 'test' sets and set the split-ratio to be 70%
# b. Apply feature scaling on all the columns of 'train' & 'test' set, except the 
# 'loan_decision_status' column
# c. Apply principal component analysis on the first 7 columns of 'train' & 'test' 
# set. The number of principal components obtained should be 2 
# d. Build the naïve bayes model on the train set 
# e. Predict the values on the test set 
# f. Build a confusion matrix for actual values and predicted values

loan_split <- sample.split(customer_loan_refined$loan_decision_status,SplitRatio = 0.7)
train <- subset(customer_loan_refined,loan_split==T)
test <- subset(customer_loan_refined,loan_split==F)

traintest_combined <- rbind(train,test)

PCA_scaling <- prcomp(traintest_combined[,-8], scale. = T)

summary(PCA_scaling)

plot(PCA_scaling)


names(PCA_scaling)

PCA_scaling$center

PCA_scaling$rotation[1:5,]

biplot(PCA_scaling, scale = 0)

plot(PCA_scaling)

train.data <- data.frame(loan_decision_status=traintest_combined$loan_decision_status,
                         PCA_scaling$x)
### Select only 2 PCA
train.data <- train.data[,1:3]
train.data$loan_decision_status <- as.factor(train.data$loan_decision_status)


loan_split_tag <- sample.split(train.data$loan_decision_status,SplitRatio = 0.7)
train1 <- subset(train.data,loan_split_tag==T)
test1 <- subset(train.data,loan_split_tag==F)


naive_model <- naiveBayes(train1[,-1],train1$loan_decision_status)
loan_predict <-predict(naive_model,newdata = test1)

table(loan_predict,test1$loan_decision_status)
confusionMatrix(loan_predict,test1$loan_decision_status)


