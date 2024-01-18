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
file.sources = list.files( c("./functions","./models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


## Epinow2, Table S2
mywave = "Omicron"
BP_RIDE_pf = BP_RIDE_perform("arima")
arima_pf = model_performance("arima")[,-c(1,2)]
df = rbind(BP_RIDE_pf,arima_pf) %>% filter(model!= "ENSEMBLE")


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
    model = factor(model, levels =  c("ARIMA","RIDE","BP","Epinow2")) #"ENSEMBLE"
  ) %>% 
  arrange(desc(type),desc(period),model) %>% 
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
    model = factor(model, levels =  c("ARIMA","RIDE","BP","Epinow2")) #"ENSEMBLE"
  ) %>% 
  arrange(desc(period),model) %>% 
  mutate(across(where(is.numeric), round, digits=2))
avg_all$type = "Overall"
avg_final = rbind(avg_df,avg_all)


avg_final_wide = 
  avg_final %>% pivot_wider(names_from = period,values_from = c(rmse,mape,auc)) %>% 
  dplyr::select(model,type,rmse_train,mape_train,auc_train,rmse_test,mape_test,auc_test,rmse_valid,mape_valid,auc_valid)
write.csv(avg_final_wide ,"./tidy_result/tidy_res/Omicron_epinow2_wide.csv",row.names = F)



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
  ) 
pf_wide
