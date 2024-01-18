#rm(list = ls())
library(Rcpp)
library(RcppParallel)
library(chron)
library(mvtnorm)
library(incidental)
library(tidyverse)
library(surveillance)
########
# set link
setwd("./generate_rt/generate_true_rt")
########
## function to compute the mcmc output
para_summary <- function(mcmc,a,b,print){
  y <- matrix(NA,ncol(mcmc),4)
  for (i in 1:ncol(mcmc)){
    y[i,1:3] <- quantile(mcmc[,i],c(0.5,0.025,0.975))
    y[i,4] <- sum(diff(mcmc[,i])!=0)/nrow(mcmc)
  }
  y[i,1] <- mean(mcmc[,i])
  layout(matrix(1:(a*b),nrow=a,byrow=T))
  par(mar=c(2,4,1,1))
  if (print==1){
    for (i in 1:ncol(mcmc)){
      plot(mcmc[,i],type="l")
    }
  }
  return(y)
}

sourceCpp("rt.cpp")
  
for (uuuu in 1:200){ 
  
  ## simulate! 
  
  ## here sample the incubation meanlog and sdlog
  meanvec <- c(1.4341,0.6613)
  
  #####################################################################
  ## here sample the incubation meanlog and sdlog
  incpara <- meanvec
  incvec <- (plnorm(1:20,incpara[1],incpara[2])-plnorm(0:19,incpara[1],incpara[2]))/plnorm(20,incpara[1],incpara[2])
  
  ## infectiousness, shifted gamma distribution f_c(x) = f(x+c) with parameters 
  infpara <- c(20.52,1.59,12.27)
  infvec <- (pgamma(1:40,infpara[1],infpara[2])-pgamma(0:39,infpara[1],infpara[2]))
  
  w_dis1 <- rep(0,20)
  for (i in 1:20){
    for (j in 1:40){
      if (i+j-12>0 &(i+j-12<=20)){  
        w_dis1[i+j-12] <- w_dis1[i+j-12] + incvec[i]*infvec[j]  
      }
    }  
  }
  w_dis2 <- w_dis1
  w_dis2[1:5] <- 0
  w_dis <- cbind(w_dis2/sum(w_dis2),w_dis1/sum(w_dis1))
  
  # ## read data1 here
  # data <- read.csv("data_all.csv")  ## 
  
  
  ## data1
  
  refdate <- dates("09/20/2020") 
  data <- read.csv("data_all_Dec.csv") %>% mutate(date = as.Date(date))
  data <- data[218:410,]   ## using 234: 440 Rt
  diff_days = dates(as.character(data$date[nrow(data)]),format="y-m-d") - refdate +1
  
  dt= data.frame(
    confirm.date = dates(as.character(data$date),format="y-m-d") - refdate ,
    type = "local",
    pcr_local_daily = data$confirm
  )
  
  a = dt %>% 
    uncount(pcr_local_daily)
  a1 <- a[sample(1:nrow(a),replace=T),]
  a11 <- matrix(NA,diff_days,4)
  
  for (i in 1:nrow(a11)){
    a11[i,3] <- sum(a1$confirm.date==i,na.rm=T)
  }
  data1 <- a11
  
  
  
  ## delay
  delay <- read.csv("delay.csv")  ### ???based on the 3rd wave?
  #sourceCpp("./rt.cpp")
  ## 
  
  ## generate convolution
  rint <- matrix(0,20,4)
  
  for (k in 1:4){
    for (i in 1:20){
      for (j in 1:20){
        if (i+j>0&(i+j<=20)){
          rint[i+j-1,k] <- rint[i+j,k] + incvec[i]*delay[j,k]
        }
      }
    }
  }
  
  rint[,3] <- rint[,3]/sum(rint[,3])
  delay_dist = rint[,3] 
  
  incidence_model = fit_incidence(
    reported = data1[,3],
    delay_dist = delay_dist,
    dof_grid = seq(22, 26, 2),
    lam_grid = 10^(seq(1,-4, -1))
  )
  
  inci_dat = matrix(0,nrow(data1),4)
  inci_dat[,3] <- incidence_model$Ihat
  
  inci_dat1 <- inci_dat
  
  inci_dat1 <- inci_dat
  for (i in 1:nrow(inci_dat)){
    for (j in 1:ncol(inci_dat)){
      inci_dat1[i,j] <- round(rpois(1,inci_dat[i,j]))  
      #data11[i,j] <- round(data1[i,j])
    }  
  }

  
  smooth <- 7
  startpt <- 24

  
  # first 10 is to resevere for parameters other than Rt
  para <- c(rep(0,10),rep(1,nrow(data1)-startpt+1))
  # for para2, also restricted to <= 153
  para2 <- c(0.1)
  
  
  move <- rep(1,length(para))
  #move[1:(length(para)-40)] <- 0
  move[1:10] <- 0
  para[1:10] <- 0
  
  sigma <- (abs(para)+0.1)/5
  sigma2 <- (abs(para2)+0.1)/5
  
  aaaaa1 <- Sys.time()
  tt <- mcmc(inci_dat1,para,para2,w_dis,smooth,startpt,5000,move,sigma,sigma2)
  aaaaa2 <- Sys.time()
  print(aaaaa2-aaaaa1)
  
  ##plot 
  id <- runif(1,0,1) + 10
  inc <- 1000+1:4000
  z1 <- para_summary((tt[[1]][inc,]),4,3,1)
  # plot(tt[[2]][inc,1])
  # print(z1)

  
  write.csv(z1,paste("./true_rt_wave4/mcmc_summary_test_",uuuu,"_",id,".csv",sep=""),row.names = F)
}



