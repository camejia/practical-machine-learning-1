library(ggplot2)
library(caret)
library(randomForest)
library(cvTools)
library(lattice)
library(robustbase)
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
training <- read.csv("pml-training.csv");
test  <- read.csv('pml-testing.csv');

# divide to training data or testing data(the partition is based on 60%,40%)
trainIndex <- createDataPartition(y = training$classe, p=0.6,list=FALSE);
trainingPartition <- training[trainIndex,];
testingPartition <- training[-trainIndex,];

#model
set.seed(3433)
#parallel computing for multi-core
registerDoParallel(makeCluster(detectCores()))
#three models are generated:  random forest   ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model   
model_rf <- train(classe ~ .,  method="rf", data=trainingPartition)    
model_gbm <-train(classe ~ ., method = 'gbm', data = trainingPartition)
model_lda <-train(classe ~ ., method = 'lda', data = trainingPartition) 

# accuracy in different models
print("Random forest accuracy ")
rf_accuracy<- predict(model_rf, testingPartition)
print(confusionMatrix(rf_accuracy, testingPartition$classe))
print("Boosted trees accuracy ")
gbm_accuracy<- predict(model_gbm , testingPartition)
print(confusionMatrix(gbm_accuracy, testingPartition$classe))
print("Linear discriminant analysis")
lda_accuracy<- predict(model_lda , testingPartition)
print(confusionMatrix(lda_accuracy, testingPartition$classe))

# 10 cross validation 
set.seed(3433)
#parallel computing for multi-core
registerDoParallel(makeCluster(detectCores()))  
controlf <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
model_rf_CV <- train(classe ~ ., method="rf",  data=trainingPartition, trControl = controlf)
print("Random forest accuracy after CV")
rf_CV_accuracy<<- predict(model_rf_CV , testingPartition)
print(confusionMatrix(rf_CV_accuracy, testingPartition$classe))
# analyze the variables' importance 
print("Variables importance in model")
vi = varImp(model_rf_CV$finalModel)
vi$var<-rownames(vi)
vi = as.data.frame(vi[with(vi, order(vi$Overall, decreasing=TRUE)), ])
rownames(vi) <- NULL
print(vi)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


#Prediction Assignment Submission
predictionassignmet<- function(){
  prediction <- predict(model_rf_CV, test)
  print(prediction)
  answers <- as.vector(prediction)
  pml_write_files(answers)
}
