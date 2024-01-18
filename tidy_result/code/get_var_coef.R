library(tidyverse)
library(forecast)
library(data.table)
library(rlist)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(latex2exp)
library(ggh4x)
library(lmtest)
file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

modelname = "arima"
model_fit <<- Fit_functions[[modelname]]
## Extract coef
train_start_date<- as.Date("2022-02-18")
train_analysis_date<-c(seq(as.Date("2022-03-01"),as.Date("2022-06-30"), by="day"))
var_final_epi_cov = read.csv(paste("./model_res/Omicron/vars_cov/",modelname,"_var_epi_cov.csv",sep = ""))[,1]
Omi_epi_cov = model_epi_cov(start.date = train_start_date,
                               date.analyse = train_analysis_date,
                               predictors = var_final_epi_cov,modelname = modelname)

Omi_epi_cov_all = NULL
for( i in 1:length(train_analysis_date)){
  each_fit = Omi_epi_cov$fit_all[[i]]
  df = data.frame(vars = c("temp_rt",var_final_epi_cov),
                  mean = coef(each_fit)[c("temp_rt",var_final_epi_cov)],
                  confint(each_fit)[c("temp_rt",var_final_epi_cov),],
                  model = "ARIMA",
                  wave = "Omicron",
                  date = train_analysis_date[i])
  Omi_epi_cov_all = rbind(Omi_epi_cov_all,df)
}



all_df = Omi_epi_cov_all
all_df$vars = ifelse(all_df$vars == "temp_rt", "rt_temp_past_0",all_df$vars)
colnames(all_df) = c("vars","mean","lower","upper","model","wave","date")

saveRDS(all_df,"./tidy_result/tidy_res/all_coef.rds")

