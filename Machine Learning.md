---
title: "Practical Machine Learning"
author: "Brayan Alexis Avila Xochihuatl"
date: "15 de mayo de 2019"
output: html_document
---
##Read the data.

We will start loading our workspace:
```{r}
getwd()
setwd("C:/Users/Krato/Desktop/Coursera/Practical ML")
save.image("C:\\Users\\Krato\\Desktop\\Coursera\\Practical ML\\.RData")
```
Now we will read the data and observe its dimension:
```{r}
test<- read.csv("pml-testing.csv")
train<- read.csv("pml-training.csv")
dim(test)
dim(train)
```
##Data Cleaning

We now delete columns (predictors) of the training set that contain any missing values.
```{r}
train <- train[, colSums(is.na(train)) == 0] 
test <- test[, colSums(is.na(test)) == 0]
classe <- train$classe
trainR <- grepl("^X|timestamp|window", names(train))
train <- train[, !trainR]
trainM <- train[, sapply(train, is.numeric)]
trainM$classe <- classe
testR <- grepl("^X|timestamp|window", names(test))
test<- test[, !testR]
testM <- test[, sapply(test, is.numeric)]
```
##Data Partition

```{r}
library(caret)
set.seed(12345) 
inTrain <- createDataPartition(trainM$classe, p=0.70, list=F)
train_data <- trainM[inTrain, ]
test_data <- trainM[-inTrain, ]
plot(train_data$classe, col="brown", main="Frequency of different levels", xlab="classe", ylab="Frequency")
```
##Data Prediction and Modelling
```{r}
library(randomForest)
library(rpart)
setting <- trainControl(method="cv", 5)
RandomForest <- train(classe ~ ., data=train_data, method="rf", trControl=setting, ntree=250)
library(e1071)
library(gbm)
```
#Random Forest
```{r}
model1 <- randomForest(classe ~., data=train_data, method="class")
 # Predicting
prediction1 <- predict(model1,test_data, Type="class")
# Testing
predictTest <- predict(model1, test_data)
predictTest
```
# Create files for submission
```{r}
pml_write_files = function(x){
n = length(x)
for(i in 1:n){
filename = paste0("problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}
pml_write_files(predictTest)
```
