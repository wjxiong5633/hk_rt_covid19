arima_fit = function(training.set){
  mytry = try(auto.arima(training.set$true_rt,
                         xreg = data.matrix(training.set[,-1]),
                         stationary = T),
              silent = T)
  if(inherits(try(mytry$fitted,silent = T),"try-error")){
    print("error")
    model = NA
  }else{
    model = mytry
  }
  return(model)
}


