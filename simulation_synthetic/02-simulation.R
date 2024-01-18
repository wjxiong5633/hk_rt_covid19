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


#################-----------------shift Report
library(data.table)
mean_delay = delay_dist%*% c(1:30)
report_shift_case = shift(reportEpiCurve,-mean_delay,fill = NA)

rtdf_reportshift = data.frame(time = 1:(mt-1),incidence_report_shift = report_shift_case)

cori_reportshift_rt = get_cori(rtdf_reportshift, icol_name = 'incidence_report_shift', window = wdw,  out_name = 'Cori',
                               GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_reportshift_rt) = c("time","rt","rt_lwr","rt_upr")




#################-----------------RIDE deconv
library(incidental)
ride_res = fit_incidence(reported = reportEpiCurve,delay_dist = delay_dist)
deconv_case = ride_res$Ihat
rtdf_RIDE = data.frame(time = 1:(mt-1) ,incidence_RIDE = deconv_case)
cori_RIDE_rt = get_cori(rtdf_RIDE, icol_name = 'incidence_RIDE', window = wdw,  out_name = 'Cori',
                               GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_RIDE_rt) = c("time","rt","rt_lwr","rt_upr")

dt_case = data.frame(time = 1:(mt-1), true_case = rtdf$incidence[1:(mt-1)],report_case = reportEpiCurve,report_shift_case = report_shift_case,deconv_case = deconv_case)


##-------------------------- BP
decondata <- matrix(0,(mt-1),4)
data1 =  matrix(0,(mt-1),4)
data1[,3] = reportEpiCurve
temp <- sts(data1[,3])
bpnp.control <- list(k=0,eps=rep(1,2),iter.max=rep(250,2),B=-1,verbose=TRUE)
temp2 <- backprojNP(temp ,incu.pmf= delay_dist,       ##c(rint[1:14,3]/sum(rint[1:14,3])),
                    control=modifyList(bpnp.control,list(eq3a.method="C")), ylim=c(0,max(X,Y)))
print(sum(temp2@upperbound)-sum(data1[,3]))
output <- (temp2@upperbound)/sum(temp2@upperbound)*sum(data1[,3])
decondata[,3] <- output
dt_case$BP_case = decondata[,3]

rtdf_BP = data.frame(time = 1:(mt-1),incidence_BP = dt_case$BP_case)
cori_BP_rt = get_cori(rtdf_BP, icol_name = 'incidence_BP', window = wdw,  out_name = 'Cori',
                        GI_mean=parlist$true_mean_GI,  GI_var=2*(parlist$true_mean_GI/2)^2, wend = TRUE)
colnames(cori_BP_rt) = c("time","rt","rt_lwr","rt_upr")
colors = c("Infections" = "#E68B81", "Report" =  "#8481BA" , "Deconv-RIDE" = "#7DA6C6", "Deconv-BP" = "#FBD178" , #"#f0d580"
           "Report-shift" =  "#84C3b7" )

true_rt = data.frame(time = 1:(mt-1),true_rt = rtdf$true_rt[1:(mt-1)])
save.image(file = "./data/mysim.Rdata")





