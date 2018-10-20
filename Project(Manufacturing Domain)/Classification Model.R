setwd(choose.dir())
library(readxl)
data<-read_excel('Defect analysis sample Sheet - Copy.xlsx')
data<-data[-c(1,2),]
colnames(data)<-as.character(data[1,])
data<-data[-1,]
data<-data[,c(2:11)]
data$Shift<-as.factor(data$Shift)
data$`Line no.`<-as.factor(data$`Line no.`)
data$Product<-as.factor(data$Product)
data$`Defect Name`<-as.factor(data$`Defect Name`)
data$`Section No.`<-as.factor(data$`Section No.`)
data$`Primary Cause`<-as.factor(data$`Primary Cause`)
data$`Root Cause Category`<-as.factor(data$`Root Cause Category`)
data$`Operator Name`<-as.factor(data$`Operator Name`)
data$`Hour no.`<-as.numeric(data$`Hour no.`)
data$`No. of Occurance`<-as.numeric(data$`No. of Occurance`)

# Random Forest prediction of Root cause data
#random forest is not biased towards one class ie it takes care of unbalanced data.

library(caret)
attach(data)
set.seed(100)
trainDataIndex <- createDataPartition(data$`Root Cause Category`, p=0.7, list = F)  # 70% training data
trainData <- data[trainDataIndex, ]
testData <- data[-trainDataIndex, ]
table(testData$`Root Cause Category`)
library(randomForest)

#Tune randomForest for the optimal mtry parameter
#search for the optimal value 
#(with respect to Out-of-Bag error estimate) of mtry for randomForest.

bestmtry<-tuneRF(trainData[,-10],as.factor(trainData$`Root Cause Category`),stepFactor = 1.2,improve = 0.01,trace = TRUE,plot = TRUE)

#stepFactor-- at each iteration, mtry is inflated (or deflated) by this value
#improve --the (relative) improvement in OOB error must be by this much for the search to continue
#trace--whether to print the progress of the search
#plot--whether to plot the OOB error as function of mtry
#doBest	--whether to run a forest using the optimal mtry found--options to be given to randomForest
#If doBest=TRUE, it returns the randomForest object produced with the optimal mtry.


attach(trainData)
fit <- randomForest(`Root Cause Category`~.,data=trainData)
print(fit) # view results 
fit$importance#gives gini index(priority of variables)
importance(fit) # importance of each predictor max value more imp variables
varImpPlot(fit)
plot(fit)  
votes<-as.data.frame(fit$votes)

# Predicting test data 
pred_test <-as.data.frame( predict(fit,testData))
confusionMatrix(table(pred_test$`predict(fit, testData)`,testData$`Root Cause Category`))
confusionMatrix(table(pred_test$`predict(fit, testData)`,testData$`Defect Name`))

pred_train <-as.data.frame( predict(fit,trainData))
confusionMatrix(table(pred_train$`predict(fit, trainData)`,trainData$`Root Cause Category`))

################################################################################

library(klaR)
attach(trainData)
naive.bayes<- NaiveBayes(`Root Cause Category`~., data = trainData, usekernel = FALSE, fL = 0)
#Factor for Laplace correction, default factor is 0, i.e. no correction.

predict(naive.bayes)
NB_pred<-as.data.frame(predict(naive.bayes, testData[,-10]))
confusionMatrix (table(NB_pred$class, testData$`Root Cause Category`))


##################################################################################3
##Weighted k-Nearest Neighbors
library(kknn) 
attach(trainData)
knn <- kknn(formula = formula(trainData$`Root Cause Category`~.), train = trainData, test = testData, k = 5, distance = 1)
#k	-Number of neighbors considered.
#distance	=Parameter of Minkowski distance.
fit <- fitted(knn)
confusionMatrix(table(testData$`Root Cause Category`, fit))
performance(fit,"tpr", "fpr")
#################################################################################






