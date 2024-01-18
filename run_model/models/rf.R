rf_fit = function(training.set,W){
  model = randomForest(true_rt ~ ., data = training.set[W,], ntree=200,na.action = na.omit)
  return(model)
}