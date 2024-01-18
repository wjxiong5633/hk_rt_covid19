svr_R_fit = function(training.set,W){
  model = svm(true_rt ~ ., data = training.set[W,], kernel = "radial", cost = 10, epsilon = 0.01, scale = FALSE)
  return(model)
}