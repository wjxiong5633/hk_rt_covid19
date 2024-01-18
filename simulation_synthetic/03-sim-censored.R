## Simulate synthetic data

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(EpiEstim)
library(surveillance)
setwd("./simulation_synthetic")
source('./code/util.R')
if(!dir.exists('figs')){dir.create('figs')}
parlist <- read_rds('./true_pars.rds')

mt <- 101
rtdf <- get_sim_df() %>% filter(time <= mt)

wdw <- 1 ## Set smoothing window = 1d
reset = TRUE ## If FALSE, read in cached file


cori_real_incidence_rt = get_cori(rtdf, icol_name = 'incidence', window = wdw,  out_name = 'Cori',
                                  GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE) %>% 
  filter(time<=mt-1)
colnames(cori_real_incidence_rt) = c("time","rt","rt_lwr","rt_upr")

plot(rtdf$true_rt)
lines(cori_real_incidence_rt$rt,col = "red")
plot(rtdf$incidence)

shape = 10
scale = 1
xx = 1:31;     #  Reasonable range for incubation period
prop_CDF = pgamma(xx, shape, 1/scale);
prop_PMF = prop_CDF[2:31]-prop_CDF[1:30];
prop_PMF = prop_PMF/sum(prop_PMF)
delay_dist = prop_PMF

tVec = rtdf$time
trueEpiCurve = rtdf$incidence
reportEpiCurve = rep(0, length(tVec)+length(delay_dist))
# expection of the report curve
for (i in 1:length(trueEpiCurve)){
  reportEpiCurve[i+1:30] <-   reportEpiCurve[i+1:30] + trueEpiCurve[i]*delay_dist
}

reportEpiCurve = round(reportEpiCurve)[1:(mt-1)]


#################-----------------RIDE deconv at 40
library(incidental)
ride_res_40 = fit_incidence(reported = reportEpiCurve[1:40],delay_dist = delay_dist)
deconv_case_40 = ride_res_40$Ihat
rtdf_RIDE_40 = data.frame(time = 1:40 ,incidence_RIDE = deconv_case_40)
cori_RIDE_rt_40 = get_cori(rtdf_RIDE_40, icol_name = 'incidence_RIDE', window = wdw,  out_name = 'Cori',
                        GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_RIDE_rt_40) = c("time","rt","rt_lwr","rt_upr")


#################-----------------RIDE deconv at 80
library(incidental)
ride_res_80 = fit_incidence(reported = reportEpiCurve[1:80],delay_dist = delay_dist)
deconv_case_80 = ride_res_80$Ihat
rtdf_RIDE_80 = data.frame(time = 1:80 ,incidence_RIDE = deconv_case_80)
cori_RIDE_rt_80 = get_cori(rtdf_RIDE_80, icol_name = 'incidence_RIDE', window = wdw,  out_name = 'Cori',
                           GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_RIDE_rt_80) = c("time","rt","rt_lwr","rt_upr")



#################-----------------RIDE deconv at 100
library(incidental)
ride_res = fit_incidence(reported = reportEpiCurve,delay_dist = delay_dist)
deconv_case = ride_res$Ihat
rtdf_RIDE = data.frame(time = 1:(101-1) ,incidence_RIDE = deconv_case)
cori_RIDE_rt = get_cori(rtdf_RIDE, icol_name = 'incidence_RIDE', window = wdw,  out_name = 'Cori',
                        GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_RIDE_rt) = c("time","rt","rt_lwr","rt_upr")

save.image(file = "./data/mysim_censored.Rdata")




