rm(list = ls())


library(Rcpp)
library(RcppParallel)
library(chron)
library(mvtnorm)
library(incidental)
library(surveillance)
library(tidyverse)
########
# set link
setwd("./generate_rt/generate_temp_rt")


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



## read
data <- read.csv("./data_all_Dec.csv")  %>% mutate(date = as.Date(date))
data <- data[218:410,]   ## 243 to 440          ## now 918 to 331
data11 <- as.matrix(data[,rep(4,4)]) 

for (id in 30:193){
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
  

  
  #data1 <- data11[1:id,]
  if(id< 150){
    data1 <- data11[1:id,]
  }else{
    data1 <-  data11[(id-150+1):id,]
  }
  
  delay <- read.csv("delay.csv")  ### 
  
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
  
  ## cut the data for simulation
  #data1 <- data1[1:70,]
  
  ## sim data here
  sourceCpp("./rt.cpp")
  
  delay_dist = rint[,3] 
  
  
  ### Get incidence
  incidence_model = fit_incidence(
    reported = data1[,1],
    delay_dist = delay_dist
  )
  
  inci_dat = matrix(0,nrow(data1),4)
  inci_dat[,3] <- incidence_model$Ihat
  
  
  inci_dat1 <- inci_dat
  for (i in 1:nrow(inci_dat)){
    for (j in 1:ncol(inci_dat)){
      inci_dat1[i,j] <- round(inci_dat[i,j])
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
  
  #sourceCpp("rt.cpp")
  aaaaa1 <- Sys.time()
  tt <- mcmc(inci_dat1,para,para2,w_dis,smooth,startpt,10000,move,sigma,sigma2)
  aaaaa2 <- Sys.time()
  
  print(aaaaa2-aaaaa1)
  
  #tt[[5]][378:389,]
  #tt[[6]][,483+378:389]
  #para_summary(tt[[6]][,483+378:389],4,3,0)
  
  #tt[[1]][,1] <- tt[[2]]
  
  #uuu <- runif(1,0,1)
  inc <- 1000+1:9000
  z1 <- para_summary((tt[[1]][inc,]),4,3,0)
  #plot(tt[[2]][inc,1])
  print(z1)
  
  #testing <- matrix(colMeans(tt[[4]][inc,]),ncol=4)
  
  #testing2 <- cbind(z1[11+(-4:65),1],z1[11+66+(-4:65),1])
  
  #cbind(tt[[3]],testing,testing2) -> keep
  
  #save.image(paste("test.Rdata",sep=""))
  
  write.csv(z1,paste("./temp_wave4_150days/mcmc_summary_test_",id,".csv",sep=""),row.names = F)
}
