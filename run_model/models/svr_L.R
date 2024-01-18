svr_L_fit = function(training.set,W){
  model = svm(true_rt ~ ., data = training.set[W,], kernel = "linear", cost = 5, epsilon = 0.01,scale = FALSE)
  print(model)
  return(model)
}