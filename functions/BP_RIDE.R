
BP_RIDE_perform = function(modelname){
  #modelname = "arima"
  arima_all = combine_data(modelname)
  train_df_final = arima_all %>% filter(period == "train")
  test_df_final = arima_all %>% filter(period == "test")
  valid_df_final = arima_all %>% filter(period == "valid")

  df_all = 
      data.frame( 
              period = rep(c(rep("train",8),rep("test",21),rep("valid",21)), 3),
              model = rep(c("BP_Const","RIDE_Const","Epinow2_Const"),each = 50),
              rbind( get_BPtemp_error_final_df(train_df_final),
                     get_BPtemp_error_final_df(test_df_final),
                     get_BPtemp_error_final_df(valid_df_final),
                   get_RIDEtemp_error_final_df(train_df_final),
                   get_RIDEtemp_error_final_df(test_df_final),
                   get_RIDEtemp_error_final_df(valid_df_final),
                   get_Epinow2_temp_error_final_df(train_df_final),
                   get_Epinow2_temp_error_final_df(test_df_final),
                   get_Epinow2_temp_error_final_df(valid_df_final)),
           row.names =  NULL ) %>% 
    arrange(period)
  return(df_all)

#saveRDS(BP_RIDE_all,"./data/res/BP_RIDE_performance.rds")
}
