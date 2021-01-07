library(caret)
set.seed(1)
data = read.csv("customer_churn.csv", stringsAsFactors = TRUE)
idx.train = createDataPartition(y = data$Churn, p=0.8, list = FALSE)
train = data[idx.train,]
test = data[-idx.train,]

train_numeric = train
test_numeric = test

for(i in 1:19){
  if(is.factor(train_numeric[,i])){
    train_numeric[,i]=as.numeric(train_numeric[,i])
    test_numeric[,i]=as.numeric(test_numeric[,i])
  }
}

pr.out=prcomp(train_numeric[,1:19], scale=TRUE)

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=c("blue", "red")[train$Churn],
     pch=16,xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(2,3)], col=c("blue", "red")[train$Churn],
     pch=16, xlab="Z1", ylab="Z3")

par(mfrow=c(1,2))
plot(summary(pr.out)$importance[2,], lwd=3, col="red", type="l",
     ylab="Explained variance by a PC", xlab="Number of PCs")
grid()
plot(summary(pr.out)$importance[3,], lwd=3, col="red", type="l",
     ylab="Cumulative fraction of explained variance", xlab="number of PCs")
grid()

train_z=pr.out$x[,1:12]
prd=predict(pr.out,test_numeric)
test_z=prd[,1:12]
train_z=data.frame(train_z,Churn=train$Churn)
test_z=data.frame(test_z,Churn=test$Churn)

library(randomForest)
rf=randomForest(Churn~., data=train_z, ntree=1000, importance=T)
pred=predict(rf, test_z)
mean(pred==test_z$Churn)
confMat=table(true_lab=test_z$Churn, predicted=pred)
diag(confMat)/rowSums(confMat)

rf = randomForest(Churn~., data=train, ntree=1000, importance=T)
pred=predict(rf,test)
mean(pred==test$Churn)
confMat=table(true_lab=test$Churn, predicted=pred)
diag(confMat)/rowSums(confMat)

## Asigment

test = read.csv("activity_test.csv",  stringsAsFactors = TRUE)
train = read.csv("activity_train.csv",  stringsAsFactors = TRUE)

test$activity = factor(test$activity)
train$activity = factor(train$activity)

pr.out=prcomp(train[,1:561], scale=TRUE)

plot(summary(pr.out)$importance[3,], lwd=3, col="red", type="l",
     ylab="Cumulative fraction of explained variance", xlab="number of atributes")
grid()

train_z=pr.out$x[,1:130]
prd=predict(pr.out,test)
test_z=prd[,1:130]
train_z=data.frame(train_z,activity=train$activity)
test_z=data.frame(test_z,activity=test$activity)

library(randomForest)
rf=randomForest(activity~., data=train_z, ntree=100, importance=T)
pred=predict(rf, test_z)
mean(pred==test_z$activity)
confMat=table(true_lab=test_z$activity, predicted=pred)
diag(confMat)/rowSums(confMat)


rf=randomForest(activity~., data=train, ntree=100, importance=T)
pred=predict(rf, test)
mean(pred==test$activity)
confMat=table(true_lab=test$activity, predicted=pred)
diag(confMat)/rowSums(confMat)

rf = randomForest(activity~., data=train, ntree=60, mtry=15, importance=T)
pred=predict(rf, test)
mean(pred==test$activity)
confMat=table(true_lab = test$activity, predicted=pred)
diag(confMat)/rowSums(confMat)

rf = randomForest(activity~., data=train, ntree=150, mtry=20, importance=T)
pred=predict(rf, test)
mean(pred==test$activity)
confMat=table(true_lab = test$activity, predicted=pred)
diag(confMat)/rowSums(confMat)



