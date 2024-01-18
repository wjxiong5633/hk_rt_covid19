gbm_fit = function(training.set,W){
  model = gbm(true_rt ~ ., data = training.set[W,],n.minobsinnode =3, bag.fraction = 0.97, 
              n.trees =500,verbose=FALSE)
  return(model)
}

# model_fit = gbm_fit
# 
# 
# model_fit(training.set,W)
