


epi_coef<- function(start.date = start_date,
                      date.analyse = dates_analysis,
                      predictors = var_final,
                      modelname = modelname){
  model_fit <<- Fit_functions[[modelname]]
  last_date = tail(date.analyse,1)
  C0_raw = extract.data(date.analysis = last_date, start.date = start.date)
  C0 = C0_raw
  training.set = C0 %>% 
    filter(date < last_date) %>% 
    dplyr::select(true_rt,temp_rt,all_of(predictors))
  
  fit = model_fit(training.set)
  coef_res = fit$coef
  return(coef_res)
}



epi_cov_coef<- function( start.date = start_date,
                      date.analyse = dates_analysis,
                      predictors = var_final,
                      modelname = modelname){
  
  model_fit <<- Fit_functions[[modelname]]
  last_date = tail(date.analyse,1)
  C0_raw = extract.data.cov(start.date = start.date,date.analysis = last_date)
  ###### obtain best lag
  C0_pre = C0_raw  ##%>% filter(date < da)
  ## obtain lagged-data for  fitting model
  if(sum(predictors %in% c(stringency,meteorology))>0){
    lag_predictor = intersect(predictors,c(stringency,meteorology))
    best_lag = obtain_lag(C0_pre) %>% filter(var %in% lag_predictor) 
    cov_lag = obtain_data_afterlag(best_lag,C0_raw,lag_predictor)
    epi_df = C0_raw  %>%  dplyr::select(-true_rt,-temp_rt,-all_of(c(stringency,meteorology)))   
    C0 = left_join(cov_lag,epi_df,by = "date") 
  }else{
    C0 = C0_raw
  }
  training.set = C0 %>% 
    filter(date < last_date) %>% 
    dplyr::select(true_rt,temp_rt,all_of(predictors))
  
  fit = model_fit(training.set)
  coef_res = fit$coef
  return(coef_res)
}




epi_cov_final_fit<- function( start.date = start_date,
                         date.analyse = dates_analysis,
                         predictors = var_final,
                         modelname = modelname){
  
  model_fit <<- Fit_functions[[modelname]]
  last_date = tail(date.analyse,1)
  C0_raw = extract.data.cov(start.date = start.date,date.analysis = last_date)
  epi_df = C0_raw  %>%  dplyr::select(-true_rt,-temp_rt,-all_of(c(stringency,meteorology)))   
  ###### obtain best lag
  C0_pre = C0_raw  %>% filter(date < last_date)
  ## obtain lagged-data for  fitting model
  best_lag = obtain_lag(C0_pre)
  cov_lag = obtain_data_afterlag(best_lag,C0_raw)
  C0 = left_join(cov_lag,epi_df,by = "date") 
  
  training.set = C0 %>% 
    filter(date < last_date) %>% 
    dplyr::select(true_rt,temp_rt,all_of(predictors))
  
  
  training.set = C0 %>% 
    filter(date < last_date) %>% 
    dplyr::select(true_rt,temp_rt,all_of(predictors))
  
  fit = model_fit(training.set)
  return(fit)
}