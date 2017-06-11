los<-read.delim("C:/Users/yuwei work/Documents/R/heartcleveland.csv",header=T,sep=",")
library(caret)
library(readr)
install.packages(caret)
install.packages("minqa")
install.packages("readr")
install.packages("caret")
install.packages("caret", dependencies = c("Depends", "Imports", "Suggests")) 
install.packages('RcppEigen')
install.packages('scales')
install.packages('Ime4')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("BradleyTerry2")
names(los)=c('age','sex','chestpaintype','restingbp','chol',
                        'fastingbsugar','restecg','maxheartrate','exerangina','oldpeak',
                        'slope','vessels','thal','heartdisease')
dim(los)
head(los)
los$heartdisease = as.factor(los$heartdisease)
library(caret)
a = createDataPartition(los$heartdisease,p=0.5,list=FALSE)
train = los[a,]
test = los[-a,]

tree = train(heartdisease ~ . ,data=train,method="rf")
confusionMatrix(test$heartdisease,predict(tree,newdata=test))
library(randomForest)

library(lattice)
library(ggplot2)

library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

summary(los)


library(rpart)
m_dt <- tree(heartdisease ~ ., data = train)
pred_dt <- predict(m_dt, train, type = "class")
confusionMatrix(train$heartdisease,pred_dt)[2:3]
#Decision Trees
library(caret)
set.seed(15689)
index <- createDataPartition(los$heartdisease,p = 0.7,list = F)
train <- los[index,]
test  <- los[-index,]


pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$heartdisease,pred_dt_test)

acc_dt <- confusionMatrix(pred_dt_test,test$heartdisease)$overall['Accuracy']
#linear regression

library(caret)
library(datasets)
model <- glm(heartdisease ~vessels,family=binomial(link='logit'),data=train)
#logist machine learning;

Train <- createDataPartition(los$heartdisease, p=0.6, list=FALSE)
training <- los[ Train, ]
testing <- los[ -Train, ]
library(zoo)
mod_fit1 <- train(heartdisease ~ vessels+chestpaintype+thal+age,  data=training, method="glm", family="binomial")
mod_fit2 heartdisease ~ vessels+chestpaintype+thal+age+chol+sex
mod_fit <- train(heartdisease ~ vessels,  data=training, method="glm", family="binomial")
varImp(mod_fit1)
varImp(mod_fit2)
pred = predict(mod_fit1, newdata=testing)
accuracy <- table(pred, testing[,"vessels"])
sum(diag(accuracy))/sum(accuracy)
pred = predict(mod_fit, newdata=testing)
accuracy <- table(pred, testing[,"vessels"])

sum(diag(accuracy))/sum(accuracy)


confusionMatrix(data=pred, testing$vessels)




summary(model)
anova(model, test="Chisq")
lmFit<-train(heartdisease~vessels,data=los,method="lm")


#Random Forest
set.seed(15689)

opt_mod <- tuneRF(train[-as.numeric(ncol(train))],train$heartdisease,ntreeTry = 150, 
                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)
mtry_fin <- opt_mod[as.numeric(which.min(opt_mod[,"OOBError"])),"mtry"]

rf_fin <- randomForest(heartdisease~.,data=train, mtry=mtry_fin, ntree=101, 
                       keep.forest=TRUE, proximity=TRUE, importance=TRUE,test=test)

pred_test <- predict(rf_fin, newdata = test)
confusionMatrix(test$heartdisease,pred_test)

acc_rf <- confusionMatrix(test$heartdisease,pred_test)$overall['Accuracy']

par(mfrow=c(1,2))
varImpPlot(rf_fin,type = 2,main = "Variable Importance",col = 'black')
plot(rf_fin,main = "Error vs no. of trees grown")

