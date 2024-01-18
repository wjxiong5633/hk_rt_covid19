combine_data = function(modelname){
  model_train_all = readRDS(paste("./model_res/Omicron/train/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  model_train_all$wave = "Omicron"
  model_train_all$period = "train"
  ## Test
  model_test_all = readRDS(paste("./model_res/Omicron/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  model_test_all$wave = "Omicron"
  model_test_all$period = "test"
  
  ## Validation
  model_valid_all = readRDS(paste("./model_res/wave4/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  model_valid_all$wave = "wave4"
  model_valid_all$period = "valid"
  
  
  data_all = rbind(model_train_all,model_test_all,model_valid_all)

  
  ## EPINOW2
  epinow2_full_new_1231 <- readRDS("./data/epinow2_rt.rds") %>%  mutate(date_analysis = as.Date(date_analysis))
  epinow2_rt = epinow2_full_new_1231[,c("date","date_analysis","rt_temp")]
  names(epinow2_rt) = c("date","date_analysis","rt_temp_epinow2")
  epinow2_rt_thatday = epinow2_rt %>% filter(date == date_analysis) %>% dplyr::select(-date_analysis)
  
  # Train
  forecast_train = model_train_all %>% filter(proj>=0)
  forecast_train = left_join(forecast_train,epinow2_rt, by = c("date","date_analysis"))
  forecast_train$rt_temp_epinow2 <- na.locf(forecast_train$rt_temp_epinow2)
  df_train = forecast_train%>% arrange(date_analysis,date)
  
  # Test
  nowcast_test = model_test_all %>% filter(proj<0)
  nowcast_test = left_join(nowcast_test,epinow2_rt_thatday, by = c("date"))
  forecast_test = model_test_all %>% filter(proj>=0)
  forecast_test = left_join(forecast_test,epinow2_rt, by = c("date","date_analysis"))
  forecast_test$rt_temp_epinow2 <- na.locf(forecast_test$rt_temp_epinow2)
  df_test = rbind(nowcast_test,forecast_test) %>% arrange(date_analysis,date)
  
  # Validation
  nowcast_valid = model_valid_all %>% filter(proj<0)
  nowcast_valid = left_join(nowcast_valid,epinow2_rt_thatday, by = c("date"))
  forecast_valid = model_valid_all %>% filter(proj>=0)
  forecast_valid = left_join(forecast_valid,epinow2_rt, by = c("date","date_analysis"))
  forecast_valid$rt_temp_epinow2 <- na.locf(forecast_valid$rt_temp_epinow2)
  df_valid = rbind(nowcast_valid,forecast_valid) %>% arrange(date_analysis,date)
  
  
  data_all_sub = rbind(df_train,df_test,df_valid) 
  
  
  ## Add cases
  case_data  = read_rds("./data/data_rt_cov_all.rds")[,1:2] %>% mutate(
    date = as.Date(date)
  )
  
  data_all = left_join(data_all_sub,case_data,by = "date")
  
  data_all$modelname = modelname
  
  data_all = data_all %>% 
    dplyr::select(date_analysis, date, proj, true_rt, pred_rt,rt_temp_RIDE,rt_temp_BP,rt_temp_epinow2,
                  wave,period,confirm,modelname)
  
  saveRDS(data_all,paste("./model_res/combine_res/",modelname, "all.rds",sep = ""))
  return(data_all)
}
