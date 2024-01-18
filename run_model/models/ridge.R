
ridge_fit = function(training.set,W){
  # control = trainControl(method="cv", number= 5)
  # tune.grid = expand.grid(lambda = 5 ^ seq(-2, -1, length = 100), alpha = c(0,1))
  # model <- train(true_rt ~., data = training.set[W,], method = "glmnet",
  #                preProcess=c('scale', 'center'),
  #                tuneGrid = tune.grid,
  #                trControl=control)
  lambdas_to_try <- 10^seq(-3, 5, length.out = 50)
  # Setting alpha = 0 implements ridge regression
  ridge_cv <- cv.glmnet(x = as.matrix(training.set[W,-1]), y = training.set[W,]$true_rt, 
                        alpha = 0, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 10)
  
  lambda_cv <- ridge_cv$lambda.min
  # Fit final model, get its sum of squared residuals and multiple R-squared
  model_cv <- glmnet(x = as.matrix(training.set[W,-1]), y = training.set[W,]$true_rt,
                     alpha = 0, lambda = lambda_cv, standardize = TRUE)
  return(model_cv)
}
