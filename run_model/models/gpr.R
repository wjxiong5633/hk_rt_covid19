gpr_fit = function(training.set,W){
  model = gausspr(true_rt ~ ., data = training.set[W,], 
              type='regression',variance.model=TRUE,kernel='rbfdot',)
  return(model)
}