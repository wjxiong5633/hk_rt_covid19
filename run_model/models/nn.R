nn_fit = function(training.set,W){
  model = nnet(true_rt ~ ., data = training.set[W,],
       size=round(nrow(training.set)/(3*ncol(training.set))),
       linout=T,maxit=1000,decay=0.01,trace=F,MaxNWts = 84581)
  return(model)
}