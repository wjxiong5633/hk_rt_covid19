library(lubridate)
library(tidyverse)
library(nnet)
library(caret)
library(tidyverse)
library(forecast)
library(data.table)
library(rlist)
library(randomForest)
library(gbm)
library(xgboost) 
library(glmnet)
library(kernlab)
library(e1071)
library(mgcv)
library(pROC)
library(zoo)
file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

Fit_functions = list(nn = nn_fit,rf = rf_fit,arima = arima_fit,gbm = gbm_fit,
                     ridge = ridge_fit, lm = lm_fit, gam = gam_fit,
                     gpr = gpr_fit,svr_L = svr_L_fit,svr_R = svr_R_fit)


## Load the data 
data_rt_cov_all <<- read_rds("./data/data_rt_cov_all.rds")
stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
               "ContainmentHealthIndex_WeightedAverage","EconomicSupportIndex")
meteorology<<- c("temperature","humidity","wind_speed")
var_need_select = c(stringency,meteorology,names(data_rt_cov_all)[5:18])


## RIDE and BP model
ride_rt = read_rds("./data/ride_rt.rds")
bp_rt = read_rds("./data/bp_rt.rds") 
ride_rt_thatday = ride_rt %>% 
  filter(date == date_analysis) %>% 
  dplyr::select(-date_analysis)
bp_rt_thatday = bp_rt %>% 
  filter(date == date_analysis) %>% 
  dplyr::select(-date_analysis)



run_model_wave4 = function(modelname){
  true_rt_dat <<- data_rt_cov_all %>% dplyr::select(date,true_rt)
  model_fit <<- Fit_functions[[modelname]]
  var_final = read.csv(paste("./Omicron/vars_cov/",modelname,"_var_epi_cov.csv",sep = ""))[,1]

  
  test_start_date <- as.Date("2020-11-29")
  test_analysis_date <-c(seq(as.Date("2021-01-01"),as.Date("2021-03-31"), by="day"))
  model_res_test = model_epi_cov_new(start.date = test_start_date,
                                     date.analyse = test_analysis_date,
                                     predictors = var_final,modelname = modelname)
  
  
  model_res_test_df = model_res_test$res_final %>% 
    dplyr::rename("date_analysis" = "date","date" = "date_proj") %>% 
    dplyr::select(date_analysis,date,proj,pred_rt)
  
  test_df = get_final_df_7d_test(model_res_test_df,true_rt_dat)
  
  test_df_final = test_df
  print(get_error_final_df(test_df_final))
  colMeans(get_error_final_df(test_df_final))
  print(get_RIDEtemp_error_final_df(test_df_final))
  colMeans(get_RIDEtemp_error_final_df(test_df_final))
  print(get_BPtemp_error_final_df(test_df_final))
  colMeans(get_BPtemp_error_final_df(test_df_final))
  
  saveRDS(test_df_final,paste("./model_res/wave4/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  
  return(test_df_final)
}

arima_res = run_model_wave4("arima")
nn_res = run_model_wave4("nn")
svr_L_res = run_model_wave4("svr_L")
gpr_res = run_model_wave4("gpr")
rf_res = run_model_wave4("rf")
gbm_res = run_model_wave4("gbm")
gam_res = run_model_wave4("gam")
ridge_res = run_model_wave4("ridge")
