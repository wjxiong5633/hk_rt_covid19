
elastic_fit = function(training.set,W){
  control = trainControl(method="cv", number= 5)
  tune.grid = expand.grid(alpha=seq(0,1,length=5),
                          lambda = seq(0.0001,0.2,length=5))
  model <- train(true_rt ~., data = training.set[W,], method = "glmnet",
                 preProcess=c('scale', 'center'),
                 tuneGrid = tune.grid,
                 trControl=control)
  return(model)
}
