pred_fun = function(modelname,fit,df){
  alpha =  1/2*c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  alpha2= 1-alpha
  if(sum(!is.na(fit)) ==0){
    predicted_mean = rep(1000,nrow(df))
    pred_rt_lwr = array(data=1000, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
    pred_rt_upr = array(data=1000, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
    colnames(pred_rt_lwr) = str_c("pred_rt_lwr",1:11)
    colnames(pred_rt_upr) = str_c("pred_rt_upr",1:11)
    dd = cbind(predicted_mean =predicted_mean,pred_rt_lwr,pred_rt_upr) %>% as.data.frame()
  }else{
    if(modelname %in% c("arima","arima2")){
      pred_arima = forecast(fit,xreg = data.matrix(df),
                            level = c(10,20,30,40,50,60,70,80,90,95,98))
      
      ##pred_arima = forecast(fit,xreg = rep(data.matrix(rep(df[,-1],nrow = 3))
      
      predicted_mean = ifelse(pred_arima$mean<0,0, pred_arima$mean)%>% as.numeric()
      pred_rt_lwr = ifelse(pred_arima$lower<0,0,pred_arima$lower) %>% as.data.frame()  #matrix(pred_arima$lower %>% as.numeric(), ncol =11) %>% as.data.frame()
      colnames(pred_rt_lwr) = str_c("pred_rt_lwr",1:11)
      pred_rt_upr = ifelse(pred_arima$upper<0,0,pred_arima$upper)%>% as.data.frame() # matrix(pred_arima$upper %>% as.numeric(), ncol =11) %>% as.data.frame()
      colnames(pred_rt_upr) = str_c("pred_rt_upr",1:11)
      dd = cbind(predicted_mean =predicted_mean,pred_rt_lwr,pred_rt_upr) %>% as.data.frame()
    }else if(modelname %in% "gam"){
      proj<-predict(fit,newdata = df,se.fit=TRUE)
      predicted_mean = proj$fit
      pred_rt_lwr =  array(data=NA, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
      pred_rt_upr =  array(data=NA, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
      for (i in 1:length(alpha)){ 
        pred_rt_lwr[,i]<-proj$fit+qnorm(alpha[i])*proj$se.fit
        pred_rt_upr[,i]<-proj$fit+qnorm(alpha2[i])*proj$se.fit
      }
      colnames(pred_rt_lwr) = str_c("pred_rt_lwr",1:11)
      colnames(pred_rt_upr) = str_c("pred_rt_upr",1:11)
      dd = cbind(predicted_mean =predicted_mean,pred_rt_lwr,pred_rt_upr) %>% as.data.frame()
    }else{
      n.boostrap = 100
      ### Other models
      pred=array(data=NA, dim=c(100,nrow(df)))
      for(k in 1:n.boostrap){
        if(modelname == "ridge"){
          pred[k,] = predict(object=fit[[k]], newx = as.matrix(df))[,1]
        }else{
          pred[k,] = predict(object=fit[[k]], newdata = df)
        }
      }
      #mean
      predicted_mean = colMeans(pred)
      #CI
      pred_rt_lwr =  array(data=NA, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
      pred_rt_upr =  array(data=NA, dim = (c(nrow(df),c(length(alpha))))) %>% as.data.frame()
      for(i in 1:length(alpha)){
        pred_rt_lwr[,i] = apply(pred, 2,quant2,a=alpha2[i])
        pred_rt_lwr[,i] = ifelse(pred_rt_lwr[,i]<0,0,pred_rt_lwr[,i])
        pred_rt_upr[,i] = apply(pred, 2,quant2,a=alpha2[i])
        pred_rt_upr[,i] = ifelse(pred_rt_upr[,i]<0,0,pred_rt_upr[,i])
      }
      colnames(pred_rt_lwr) = str_c("pred_rt_lwr",1:11)
      colnames(pred_rt_upr) = str_c("pred_rt_upr",1:11)
      dd = cbind(predicted_mean =predicted_mean,pred_rt_lwr,pred_rt_upr) %>% as.data.frame()
    }
  }
  return( dd )
}
