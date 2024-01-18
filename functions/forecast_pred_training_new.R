
extract.data.cov <- function(data.file= data_rt_cov_all,
                             start.date = start_date,
                             date.analysis){
  D<-data.file %>%
    filter(date >=  start.date & date<= date.analysis) 
  
  
  return(D)
}

model_epi_cov<- function(start.date = start_date,
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
    C0_pre = C0_raw 
    ## obtain lagged-data for  fitting model
    if(sum(predictors %in% c(stringency,meteorology))>0){
      best_lag = obtain_lag(C0_pre)
      cov_lag = obtain_data_afterlag(best_lag,C0_raw)
      epi_df = C0_raw  %>%  dplyr::select(-true_rt,-temp_rt,-all_of(c(stringency,meteorology)))   
      C0 = left_join(cov_lag,epi_df,by = "date") 
    }else{
      C0 = C0_raw
    }
    
    ## Using previous days to train model
    if(sum(!is.na(predictors))>0){
      training.set = C0 %>% filter(date < da) %>% dplyr::select(true_rt,temp_rt,all_of(predictors))
    }else{
      training.set = C0 %>% filter(date < da) %>% dplyr::select(true_rt,temp_rt)
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
    
    newdata<-data.frame(date = seq.Date(date.analyse[K],date.analyse[K]+7,by="day"))
    sub1<-C0_raw[,c("date","temp_rt",all_of(predictors))]
    newdata<-left_join(newdata,sub1[,c("date","temp_rt")],by = "date")
    
    ## replace NA with last day
    sub3 = C0[,c("date","temp_rt",all_of(predictors))]%>% data.frame()
    newdata = left_join(newdata,sub3[,c("date",all_of(predictors))],by=c("date"="date"))
    for(j in 1:ncol(newdata)){
      newdata[is.na( newdata[,j]),j]= sub3[sub3$date == da-1, j ] #sub1[(nrow(sub1)-7):(nrow(sub1)-1),j] last(newdata[!is.na( newdata[,j]),j])
    }
    
    newdata = newdata %>% dplyr::select(temp_rt,all_of(predictors))
    pred_now_forecast = pred_fun(modelname,fit,newdata)
    nowcast = C0 %>% filter(date == da)
    
    
    ## Prediction
    prediction_horizon = seq(0,7)
    length.prediction = length(prediction_horizon)
    date.predictions = prediction_horizon+da
    
    D = data.frame(date = as.Date(da),
                   proj = prediction_horizon,
                   date_proj = as.Date(da) + prediction_horizon,
                   # true_rt = nowcast$true_rt,
                   # temp_rt = nowcast$temp_rt,
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



rmse_eachstep_epi_cov = function(variables,var_need_select,modelname,start_date,dates_analysis){
  res = list()
  error_df = NULL
  for(i in 1:length(var_need_select)){
    print(paste("-------------",i,var_need_select[i]))
    var_final = c(variables,var_need_select[i])
    
    each_var_res= model_epi_cov(start.date = start_date,date.analyse = dates_analysis,
                                predictors = var_final,modelname = modelname)
    
    res[[i]] = each_var_res
    sub1 = true_rt_dat  %>% distinct() %>% filter(date %in% dates_analysis)  #train date
    sub2 = each_var_res$res_final %>% dplyr::rename("date_analysis" = "date","date" = "date_proj")
    sub3 = left_join(sub1,sub2,by = "date")
    split_list = split(sub3,f = sub3$proj)
    res_i = lapply(split_list,function(df){
      get_error_df(df)
    }) %>% list.rbind()
    print(colSums(res_i))
    error_df = rbind(error_df,colSums(res_i))
  }
  error_df = as.data.frame(error_df)
  return(error_df)
}




forward_select_epi_cov = function(var_need_select,threshhold,modelname,start_date,dates_analysis){
  rmse_now = 100
  decrease_per = -100
  variables = c()
  while(decrease_per <= -threshhold){
    rmse_pre = rmse_now
    error_df =  rmse_eachstep_epi_cov(variables,var_need_select,modelname,start_date,dates_analysis)
    rmse_vec = error_df$rmse
    print(rmse_vec)
    index = which.min(rmse_vec)
    var_add = var_need_select[index]
    rmse_now = rmse_vec[index]
    decrease_per = round((rmse_now - rmse_pre)/rmse_pre,4)
    
    print(paste("Now RMSE is ",rmse_now))
    if(decrease_per <= -threshhold){
      print(paste("Decrease is ",decrease_per, ", more than ", threshhold))
      rmse_pre = rmse_now
      variables = c(variables,var_add)
      var_need_select = var_need_select[! var_need_select %in% var_add]
    }
    else{
      print(paste("Decrease is ",decrease_per, ", less than ",threshhold))
    }
    
    print(paste(c("Now we add variables ",variables),collapse  = " "))
    print(paste(c("Variables left: ",var_need_select),collapse  = " "))
    print("---------------------------")
  }
  return(var_final = variables)
}







