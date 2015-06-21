## Install package firstly
## Load library and data
load both training and test data downloading from web
## Divide training data to training and testing parts based on 60% and 40%.
training:60%, testing: 40%
## Creat mode for this question
Here, I use 3 models
* random forest
* boosted trees 
* linear discriminant analysis 

##  accuracy from each model
* random forest: 98%
* boosted trees:96%
* linear discriminant analysis:69%

Based on the performance, we choose random forest for further testing and prediction.
## 10 cross validation
Use 10 cross validation to increase the accuracy of random forest. I get around 99% accuracy.
## predicting 20 test case
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
predictionassignmet<- function(){
  prediction <- predict(model_rf_CV, test)
  print(prediction)
  answers <- as.vector(prediction)
  pml_write_files(answers)
}
