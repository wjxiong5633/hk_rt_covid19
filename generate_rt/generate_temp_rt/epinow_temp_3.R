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

########################## third wave
set.seed(45)
data <- read.csv("data_all_Dec.csv") %>% mutate(date = as.Date(date)) ## 
data <- data[95:212,c(1,4)]
  
index = which(data$date >= "2020-06-04" & data$date <= "2020-07-05")
give_1case = sample(index,15,replace = F)
data$confirm[give_1case] = 1
data$confirm[20:50]

for(id in 88:118){
  
  if(id< 100){
    reported_cases <- data[1:id,]
  }else{
    reported_cases <- data[(id-100+1):id,]
    }
  #reported_cases <- data[1:id,]
  # estimate Rt
  mylist = get_incub_gt_delay(5.2,3.9)
  generation_time = mylist$generation_time
  incubation_period = mylist$incubation_period
  reporting_delay = mylist$reporting_delay
  
  out <- epinow(reported_cases = reported_cases, 
                generation_time = generation_time,
                delays = delay_opts(incubation_period, reporting_delay),
                rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                CrIs = 0.95,
                stan = stan_opts(cores=4,control=list(adapt_delta=0.95)),
                horizon = 14, 
                target_folder = "results",
                return_output = TRUE, 
                verbose = TRUE)
  plot(out)
  
  rt <- summary(out, type = "parameters", params = "R")
  #rt = rt %>% mutate(date = as.Date(date)) %>% filter()
  
  write.csv(rt,paste("./generate_temp_rt/temp_wave3_epinow/rt_",data$date[id],".csv",sep = ""),row.names = F)
}

