
extract.data.cov <- function(data.file= data_rt_cov_all,
                             start.date = start_date,
                             date.analysis){
  D<-data.file %>%
    filter(date >=  start.date & date<= date.analysis) 
  
  
  return(D)
}

model_epi_cov_new<- function(start.date = start_date,
                             date.analyse = dates_analysis,
                             predictors = var_final,
                             modelname = modelname,
                             n.boostrap = 100){
  set.seed(123)
  alpha =  1/2*c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  alpha2= 1-alpha
  
  res=NULL
  K = 1
  predicted_mean = 0
  fit_all = list()
  while(predicted_mean != 1000 & K<= length(date.analyse)){
    da = date.analyse[K]
    #print(da)
    # All observations up to the analysis date      
    C0_raw = extract.data.cov(start.date = start.date,date.analysis = da)
    ###### obtain best lag
    C0_pre = C0_raw %>% filter(date < da-13)
    ## obtain lagged-data for  fitting model
    if(sum(predictors %in% c(stringency,meteorology))>0){
      lag_predictor = intersect(predictors,c(stringency,meteorology))
      best_lag = obtain_lag(C0_pre) 
      cov_lag = obtain_data_afterlag(best_lag,C0_raw)
      epi_df = C0_raw  %>%  dplyr::select(-true_rt,-temp_rt,-all_of(c(stringency,meteorology)))   
      C0 = left_join(cov_lag,epi_df,by = "date") 
    }else{
      C0 = C0_raw
    }
    
    ## Using previous days to train model
    if(sum(!is.na(predictors))>0){
      training.set = C0 %>% filter(date < da-13) %>% dplyr::select(true_rt,temp_rt,all_of(predictors))
    }else{
      training.set = C0 %>% filter(date < da-13) %>% dplyr::select(true_rt,temp_rt)
    }
    
    
    fit=NULL
    if(modelname %in% c("arima","arima2","gam")){
      fit = model_fit(training.set)
      fit_all[[K]] = fit
    }else{
      for(k in 1:n.boostrap){
        W =sample(x = 1:nrow(training.set),size = nrow(training.set),replace = TRUE)
        fit[[k]] = model_fit(training.set,W)
      }
    }
    
    newdata<-data.frame(date = seq.Date(date.analyse[K]-14+1,date.analyse[K]+7,by="day"))
    sub1<-C0_raw[,c("date","temp_rt",all_of(predictors))]
    newdata<-left_join(newdata,sub1[,c("date","temp_rt")],by = "date")
    
    sub3 = C0[,c("date","temp_rt",all_of(predictors))]%>% data.frame()
    newdata = left_join(newdata,sub3[,c("date",all_of(predictors))],by=c("date"="date"))
    
    nowcast_14 = newdata %>% filter(date<=da)
    for(j in 1:ncol(nowcast_14)){
      nowcast_14[is.na( nowcast_14[,j]),j]= first(nowcast_14[!is.na( nowcast_14[,j]),j])
    }
    
    forecast_7 = newdata %>% filter(date>da)
    newdata = rbind(nowcast_14,forecast_7)
    for(j in 1:ncol(newdata)){
      newdata[is.na( newdata[,j]),j]= sub3[sub3$date == da-1, j ] 
    }
    
    
    newdata = newdata %>% dplyr::select(temp_rt,all_of(predictors))
    pred_now_forecast = pred_fun(modelname,fit,newdata)
    nowcast = C0 %>% filter(date == da)
    
    
    ## Prediction
    prediction_horizon = seq(-13,7)
    length.prediction = length(prediction_horizon)
    date.predictions = prediction_horizon+da
    
    D = data.frame(date = as.Date(da),
                   proj = prediction_horizon,
                   date_proj = as.Date(da) + prediction_horizon,
                   true_rt = nowcast$true_rt,
                   temp_rt = nowcast$temp_rt,
                   pred_now_forecast,
                   modelname = modelname) %>% 
      dplyr::rename(
        "pred_rt" = "predicted_mean",
        "quantile_lwr_2"="pred_rt_lwr1",
        "quantile_lwr_5"= "pred_rt_lwr2",
        "quantile_lwr_10"= "pred_rt_lwr3",
        "quantile_lwr_20"= "pred_rt_lwr4",
        "quantile_lwr_30"= "pred_rt_lwr5",
        "quantile_lwr_40"= "pred_rt_lwr6",
        "quantile_lwr_50"= "pred_rt_lwr7",
        "quantile_lwr_60"= "pred_rt_lwr8",
        "quantile_lwr_70" = "pred_rt_lwr9",
        "quantile_lwr_80" = "pred_rt_lwr10",
        "quantile_lwr_90" = "pred_rt_lwr11",
        "quantile_upr_2"="pred_rt_upr1",
        "quantile_upr_5"= "pred_rt_upr2",
        "quantile_upr_10"= "pred_rt_upr3",
        "quantile_upr_20"= "pred_rt_upr4",
        "quantile_upr_30"= "pred_rt_upr5",
        "quantile_upr_40"= "pred_rt_upr6",
        "quantile_upr_50"= "pred_rt_upr7",
        "quantile_upr_60"= "pred_rt_upr8",
        "quantile_upr_70" = "pred_rt_upr9",
        "quantile_upr_80" = "pred_rt_upr10",
        "quantile_upr_90" = "pred_rt_upr11"
      )
    D = D %>% dplyr::select(date,proj,date_proj,pred_rt,modelname)
    res=rbind(res, D) 
    K = K +1
  }
  if(K<=length(date.analyse)){
    res_final= rbind(res, res[rep(nrow(res),length(date.analyse)-K+1),])
    res_final$date = date.analyse
  }else{
    res_final = res
  }
  return(list(res_final = res_final,
              fit_all = fit_all))
}  


