library(EpiNow2) 
library(tidyverse)
library(chron)
library(forecast)
library(rlist)
library(data.table)
file.sources = list.files( c("./functions"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
# set link
setwd("./generate_rt/generate_temp_rt")

########################## the fifth wave
data <- read.csv("data_all_Dec.csv") %>% mutate(date = as.Date(date)) ## 
data <- data[656:nrow(data),c(1,4)]

for(id in 301:415){
  if(id< 150){
    reported_cases <- data[1:id,]
  }else{reported_cases <- data[(id-150+1):id,]}
  # estimate Rt
  #reported_cases <- data[1:id,]
  mylist = get_incub_gt_delay(3.5,2.6)
  generation_time = mylist$generation_time
  incubation_period = mylist$incubation_period
  reporting_delay = mylist$reporting_delay
  
  out <- epinow(reported_cases = reported_cases, 
                generation_time = generation_time,
                delays = delay_opts(incubation_period, reporting_delay),
                rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                CrIs = 0.95,
                stan = stan_opts(cores=4,control=list(adapt_delta=0.95)),
                horizon = 7, 
                target_folder = "results",
                return_output = TRUE, 
                verbose = TRUE)
  
  rt <- summary(out, type = "parameters", params = "R")
  #rt = rt %>% mutate(date = as.Date(date)) %>% filter()
  
  write.csv(rt,paste("./generate_temp_rt/temp_wave5_epinow/rt_",data$date[id],".csv",sep = ""),row.names = F)
}

