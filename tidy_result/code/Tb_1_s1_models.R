library(tidyverse)
library(caret)
library(forecast)
library(data.table)
library(rlist)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(pROC)
library(zoo)
file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


## ARIMA, RIDE and BP models, Table 1
mywave = "Omicron"
BP_RIDE_pf = BP_RIDE_perform("arima")
arima_pf = model_performance("arima")[,-c(1,2)]
df = rbind(BP_RIDE_pf,arima_pf) %>% filter(model!= "Epinow2_Const" &model!= "ENSEMBLE")
df$type = ifelse(df$proj<=0, "nowcast","forecast")

avg_df = 
  df %>% 
  group_by(period,model,type) %>% 
  dplyr::summarise(
    rmse = mean(rmse),
    mape = mean(mape),
    auc = mean(auc) ) %>% 
  mutate(
    model = sub("\\_Const.*", "", model),
    model = factor(model, levels =  c("ARIMA","RIDE","BP")) ,
    period = factor(period,levels = c("train","test","valid"))#"ENSEMBLE"
  ) %>% 
  arrange(desc(type),period,model) %>% 
  mutate(across(where(is.numeric), round, digits=2))
avg_df

avg_all  =  df %>% 
  group_by(period,model) %>% 
  dplyr::summarise(
    rmse = mean(rmse),
    mape = mean(mape),
    auc = mean(auc) ) %>% 
  mutate(
    model = sub("\\_Const.*", "", model),
    model = factor(model, levels =  c("ARIMA","RIDE","BP")), #"ENSEMBLE",
    period = factor(period,levels = c("train","test","valid"))
  ) %>% 
  arrange(period,model) %>% 
  mutate(across(where(is.numeric), round, digits=2))
avg_all$type = "Overall"
avg_final = rbind(avg_df,avg_all)

avg_final_wide = 
  avg_final %>% pivot_wider(names_from = period,values_from = c(rmse,mape,auc)) %>% 
  dplyr::select(model,type,rmse_train,mape_train,auc_train,rmse_test,mape_test,auc_test,rmse_valid,mape_valid,auc_valid)

avg_main = avg_final %>% filter(period != "valid")

write.csv(avg_main,"./tidy_result/tidy_res/Omicron_main.csv",row.names = F)
write.csv(avg_final_wide,"./tidy_result/tidy_res/Omicron_main_wave4.csv",row.names = F)


pf_wide = avg_final %>% 
  pivot_longer(
    "rmse":"auc",
    names_to = "metrics",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = "model",
    values_from = "value"
  ) %>% 
  mutate(
    ARIMA_BP = (ARIMA-BP)/BP,
    ARIMA_RIDE = (ARIMA-RIDE)/RIDE,
    RIDE_BP = (RIDE-BP)/BP
  ) %>%  arrange(period) %>% 
  mutate(across(where(is.numeric), round, digits=2))
pf_wide


## Other models Table S1
Omi_others = rbind(
  model_performance("ARIMA"),
  model_performance("gbm"),
  model_performance("gpr"),
  model_performance("nn"),
  model_performance("rf"),
  model_performance("svr_L"),
  model_performance("ridge"),
  model_performance("gam")
 # model_performance("ensemble")
) %>% 
  dplyr::select(-type) %>% 
  mutate(
    type = ifelse(proj<=0, "nowcast","forecast")
  )

write.csv(Omi_others,"./tidy_result/tidy_res/Omicron_others.csv",row.names = F)


Omi_avg = 
  Omi_others %>% 
  group_by(modelname,period,type) %>% dplyr::summarise(
  rmse = mean(rmse),
  mape = mean(mape),
  auc = mean(auc)
) %>% 
  mutate(across(where(is.numeric), round, digits=2)) %>% 
  arrange(desc(type),period,modelname)

Omi_avg_all = 
  Omi_others %>% 
  group_by(modelname,period) %>% dplyr::summarise(
    rmse = mean(rmse),
    mape = mean(mape),
    auc = mean(auc)
  ) %>% arrange(desc(period),modelname) %>% 
  mutate(across(where(is.numeric), round, digits=2))
Omi_avg_all$type = "Overall"

Omi_final = rbind(Omi_avg,Omi_avg_all) %>% 
  filter(modelname !="ensemble")
write.csv(Omi_final,"./tidy_result/tidy_res/Omicron_others_avg.csv",row.names = F)

Omi_final_wide = 
  Omi_final %>% pivot_wider(names_from = period,values_from = c(rmse,mape,auc)) %>% 
  dplyr::select(modelname,type,
                rmse_train,mape_train,auc_train,rmse_test,mape_test,auc_test,rmse_valid,mape_valid,auc_valid)
write.csv(Omi_final_wide,"./tidy_result/tidy_res/Omicron_others_wave4.csv",row.names = F)

