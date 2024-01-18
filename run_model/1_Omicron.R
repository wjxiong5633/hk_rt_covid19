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
file.sources = list.files( c("./functions","./run_model_main/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

Fit_functions = list(nn = nn_fit,rf = rf_fit,arima = arima_fit,gbm = gbm_fit,
                     ridge = ridge_fit, lm = lm_fit, gam = gam_fit,
                     gpr = gpr_fit,svr_L = svr_L_fit,svr_R = svr_R_fit)


## Load the data (Rt and Rt_t)
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


run_model_main = function(modelname){
  true_rt_dat <<- data_rt_cov_all %>% dplyr::select(date,true_rt)
  
  model_fit <<- Fit_functions[[modelname]]
  train_start_date<- as.Date("2022-02-18")
  train_analysis_date<-c(seq(as.Date("2022-03-01"),as.Date("2022-06-30"), by="day"))
  # var_final = forward_select_epi_cov(var_need_select,threshhold = 0.01,modelname = modelname,
  #                                    start_date = train_start_date,
  #                                    dates_analysis = train_analysis_date)
  #write.csv(var_final,paste("./Omicron/vars_cov/",modelname,"_var_epi_cov.csv",sep = ""),row.names = F)
  var_final = read.csv(paste("./Omicron/vars_cov/",modelname,"_var_epi_cov.csv",sep = ""))[,1]

  model_res_train = model_epi_cov(start.date = train_start_date,
                                  date.analyse = train_analysis_date,
                                  predictors = var_final,modelname = modelname)


  model_res_train_df = model_res_train$res_final %>%
    dplyr::rename("date_analysis" = "date","date" = "date_proj") %>%
    dplyr::select(date_analysis,date,proj,pred_rt)

  train_df = get_final_df_7d_train(model_res_train_df,true_rt_dat)
  train_df_final = train_df

  print(get_error_final_df(train_df_final))
  colMeans(get_error_final_df(train_df_final))
  
  print(get_RIDEtemp_error_final_df(train_df_final))
  colMeans(get_RIDEtemp_error_final_df(train_df_final))
  # 
  # 
  saveRDS(train_df_final,paste("./Omicron/train/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
                         
                         
                         
  test_start_date <- as.Date("2022-02-18")
  test_analysis_date <-c(seq(as.Date("2022-07-01"),as.Date("2022-12-31"), by="day"))
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
  
  saveRDS(test_df_final,paste("./model_res/Omicron/test/epi_cov/",modelname,"_epi_cov_res.rds",sep = ""))
  
  return(test_df_final)
}

arima_res = run_model_main("arima")
nn_res = run_model_main("nn")
svr_L_res = run_model_main("svr_L")
gpr_res = run_model_main("gpr")
rf_res = run_model_main("rf")
gbm_res = run_model_main("gbm")
gam_res = run_model_main("gam")
ridge_res = run_model_main("ridge")
