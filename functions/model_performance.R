model_performance = function(modelname){
  test_epi_cov_res <- readRDS(paste("./model_res/Omicron/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = "")) 
  
  #test
  test_df = data.frame(
    type = c(rep("cov",21)),
    period = "test",
    model = rep(str_to_upper(modelname),each = 21),
    get_error_final_df(test_epi_cov_res),
    row.names =  NULL
  )
  
  #train
  train_epi_cov_res <- readRDS(paste("./model_res/Omicron/train/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))

  
  train_df = data.frame(
    type = c(rep("cov",8)),
    period = "train",
    model = rep(str_to_upper(modelname),each = 8),
    get_error_final_df(train_epi_cov_res),
    row.names =  NULL ) 
  
  
  valid_epi_cov_res <- readRDS(paste("./model_res/wave4/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  
  valid_df = data.frame(
    type = c(rep("cov",21)),
    period = "valid",
    model = rep(str_to_upper(modelname),each = 21),
    get_error_final_df(valid_epi_cov_res),
    row.names =  NULL ) 
  
  
  all = cbind(modelname = modelname, rbind(train_df,test_df,valid_df))%>%
    arrange(period,model,proj) 
  
  return(all)
}


model_performance_robustness = function(data_all,mywave){
  test_dat = data_all %>% filter(wave == mywave)
  #get_error_final_df(test_epi_res)
  test_epi_cov_res = test_dat 
  #get_error_final_df(test_epi_cov_res)
  
  modelname = "ARIMA"
  #test
  test_df = data.frame(
    period = "test",
    model = rep(paste(str_to_upper(modelname),"_rb",sep = ""),each = 21),
    get_error_final_df(test_epi_cov_res),
    row.names =  NULL
  ) 
  
  return(test_df)
}

