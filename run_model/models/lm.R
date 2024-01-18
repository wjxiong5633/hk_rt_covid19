lm_fit = function(training.set,W){
  model = lm(true_rt ~ ., data = training.set[W,])
  return(model)
}