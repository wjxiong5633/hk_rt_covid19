mape <- function(predicted,true){
  mape_value<- mean(abs((true - predicted)/true),na.rm = T)
  return (mape_value)
}
rmse <- function(predicted,true){
  rmse_value<- sqrt(sum((true-predicted)^2,na.rm = T)/length(true))
  return(rmse_value)
}
classify_score = function(pred,true){
  
  true_fct = factor(ifelse(true>1,1,0),levels = c(0,1))
  pred_fct = factor(ifelse(pred>1,1,0),levels = c(0,1))
  
  res = confusionMatrix(pred_fct, true_fct, mode = "everything", positive="1")
  #score = res$byClass[7]
  score = res$overall[1]
  return(score)
}


auc_calculate = function(pred,true){
  true_fct = ifelse(true>=1,1,0)  ##factor(ifelse(true>1,1,0),levels = c(0,1))
  pred_fct = ifelse(pred>=1,1,0) ##factor(ifelse(pred>1,1,0),levels = c(0,1))
  #roc_1 = roc(true_fct,pred_fct)
  auc_res = auc(true_fct,pred_fct,quiet = T) %>% as.numeric()
  return(auc_res)
}


get_error_df = function(model_res){
  error_df =
    data.frame(
      rmse = rmse(model_res$pred_rt,model_res$true_rt),
      mape = mape(model_res$pred_rt,model_res$true_rt),
      auc = auc_calculate(model_res$pred_rt,model_res$true_rt))
      #auc_2 = auc_calculate_new(model_res$pred_rt,model_res$true_rt))
  return(error_df)
}





# get_error_df_7d = function(model_res){
#   sub1 = model_res %>% dplyr::select(date,true_rt) %>% distinct()
#   sub2 = model_res %>% dplyr::select(-true_rt) %>% dplyr::rename("date_nowcast" = "date","date" = "date_proj")
#   sub3 = left_join(sub1,sub2,by = "date")
#   split_list = split(sub3,f = sub3$proj)
#   
#   error_7d_df = lapply(split_list,function(df){
#     get_error_df(df)
#   }) %>% list.rbind() %>% as.data.frame()
#   #print(colSums(res))
#   return(error_7d_df)
# }

get_final_df_7d_train = function(model_res_df,true_rt_dat){
  forecast_train = model_res_df %>% filter(proj>=0)
  forecast_train = left_join(
    left_join(forecast_train,ride_rt, by = c("date","date_analysis")),
    bp_rt, by = c("date","date_analysis"))
  forecast_train$rt_temp_RIDE <- na.locf(forecast_train$rt_temp_RIDE)
  forecast_train$rt_temp_BP <- na.locf(forecast_train$rt_temp_BP)
  
  df_train = forecast_train%>% arrange(date_analysis,date)
  
  sub1 = true_rt_dat
  sub2 = df_train
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis) 
  return(sub3)
}




get_final_df_7d_test = function(model_res_df,true_rt_dat){
  nowcast_test = model_res_df %>% filter(proj<0)
  forecast_test = model_res_df %>% filter(proj>=0)
  nowcast_test = 
    left_join(
      left_join(nowcast_test,ride_rt_thatday, by = c("date")),
      bp_rt_thatday, by = c("date"))
  forecast_test = left_join(
    left_join(forecast_test,ride_rt, by = c("date","date_analysis")),
    bp_rt, by = c("date","date_analysis"))
  forecast_test$rt_temp_RIDE <- na.locf(forecast_test$rt_temp_RIDE)
  forecast_test$rt_temp_BP <- na.locf(forecast_test$rt_temp_BP)
  
  df_test = rbind(nowcast_test,forecast_test) %>% arrange(date_analysis,date)
  
  sub1 = true_rt_dat
  sub2 = df_test
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis) 
  return(sub3)
}








get_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
          get_error_df(df) }) %>% list.rbind(),
          proj = min(model_res$proj):7)  %>% as.data.frame()
  return(error_7d_df)
}




get_RIDEtemp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      error_df = data.frame( rmse = rmse(df$rt_temp_RIDE,df$true_rt),
                             mape = mape(df$rt_temp_RIDE,df$true_rt),
                             auc = auc_calculate(df$rt_temp_RIDE,df$true_rt))
                             #auc_2 = auc_calculate_new(df$rt_temp_RIDE,df$true_rt))
      return(error_df)}) %>% list.rbind(),
      proj = min(model_res$proj):7) %>% as.data.frame()
  return(error_7d_df)
} 





get_BPtemp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      error_df = data.frame( rmse = rmse(df$rt_temp_BP,df$true_rt),
                             mape = mape(df$rt_temp_BP,df$true_rt),
                             auc = auc_calculate(df$rt_temp_BP,df$true_rt))
                             #auc_2 = auc_calculate_new(df$rt_temp_BP,df$true_rt))
      return(error_df)}) %>% list.rbind(),
      proj = min(model_res$proj):7) %>% as.data.frame()
  return(error_7d_df)
} 




get_Epinow2_temp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      error_df = data.frame( rmse = rmse(df$rt_temp_epinow2,df$true_rt),
                             mape = mape(df$rt_temp_epinow2,df$true_rt),
                             auc = auc_calculate(df$rt_temp_epinow2,df$true_rt))
                             #auc_2 = auc_calculate_new(df$epinow2_temp_rt,df$true_rt))
      return(error_df)}) %>% list.rbind(),
      proj = min(model_res$proj):7) %>% as.data.frame()
  return(error_7d_df)
} 


shift_data_cov = function(dat,t,var_final){
  sub1<- dat
  sub2<-sub1
  sub2$date<-sub2$date + t
  sub<-left_join(sub1[,c("date","true_rt")],sub2[,c("date","temp_rt",all_of(var_final))], by = c("date"))
  sub[is.na(sub$temp_rt),c("temp_rt",all_of(var_final))] = sub1[is.na(sub$temp_rt),c("temp_rt",all_of(var_final))]
  sub<-sub %>% drop_na(all_of(var_final)) %>%
    as.data.frame()
  return(sub)
}

quant2 <- function(x, a){
  quantile(x,a,na.rm = TRUE)
}

normalize <- function(x) {
  return ((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}


