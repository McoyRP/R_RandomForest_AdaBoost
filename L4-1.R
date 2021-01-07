library(caret)
library(randomForest)
data=read.csv("breastcancer.csv")
data$diagnosis = factor(data$diagnosis)

idx.train = createDataPartition(y = data$diagnosis, p = 0.8, list = FALSE) 
train = data[idx.train, ] 
test =  data[-idx.train, ] 


rf=randomForest(diagnosis~.,data=train,ntree=1000,importance=T)
plot(rf)
varImpPlot(rf)

pred=predict(rf, test)
mean(pred==test$diagnosis)
confMat=table(true_lab = test$diagnosis, predicted=pred)
diag(confMat)/rowSums(confMat)

train_label=as.numeric(train$diagnosis)-1
test_label=as.numeric(test$diagnosis)-1

library(xgboost)
bst = xgboost(data=as.matrix(train[,2:31]),
              label = train_label, num_class=2,
              max_depth = 1, nrounds = 100,
              objective = "multi:softmax")

pred = predict(bst, as.matrix(test[,2:31]))
mean(pred==test_label)
confMat=table(true_lab=test_label, predicted=pred)
diag(confMat)/rowSums(confMat)

## Uzduotis

test = read.csv("activity_test.csv")
train = read.csv("activity_train.csv")

test$activity = factor(test$activity)
train$activity = factor(train$activity)

plot(train$activity)

plot(train$tBodyAcc_Mean_1, train$tBodyAcc_STD_1, col=train$activity)
plot(train$tBodyAcc_Mad_1, train$tBodyAcc_Max_1, col=train$activity)
plot(train$tBodyAcc_Min_1, train$tBodyAcc_Energy_1, col=train$activity)

## Pirmas
rf = randomForest(activity~., data=train, ntree=150, importance=T)
pred=predict(rf, test)
mean(pred==test$activity)
confMat=table(true_lab = test$activity, predicted=pred)
diag(confMat)/rowSums(confMat)


train_label=as.numeric(train$activity)-1
test_label=as.numeric(test$activity)-1

bst = xgboost(data=as.matrix(train[,1:561]),
              label = train_label, num_class=12,
              max_depth = 15, nrounds = 35,
              objective = "multi:softmax")

wpred = predict(bst, as.matrix(test[,1:561]))
mean(wpred==test_label)
confMat=table(true_lab=test_label, predicted=wpred)
diag(confMat)/rowSums(confMat)
 

