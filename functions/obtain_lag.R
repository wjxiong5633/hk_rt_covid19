obtain_lag = function(C0_pre){
  res_lag = tibble()
  for(i in c(1:length(stringency))){  ## each var
    for (lag in c(3:7)){
      sub1 = C0_pre
      sub2 = sub1
      sub2$date<-sub2$date + lag
      sub<-left_join(sub1[,c("date","true_rt")],sub2[,c("date",stringency[i])], by = c("date")) %>% as.data.frame()
      cor<-cor.test(y=sub$true_rt,x=sub[,stringency[i]],method="pearson")$estimate
      cor<- ifelse(is.na(cor),0,cor)
      res_lag_tmp<-data.frame(pearson=cor,lag=lag,var=stringency[i])
      res_lag<-bind_rows(res_lag,res_lag_tmp)
    }
  }
  #meteorology = c("temperature","humidity","wind_speed")
  for(i in c(1:length(meteorology))){  ## each var
    for (lag in c(1:3)){
      sub1 = C0_pre
      sub2 = sub1
      sub2$date<-sub2$date + lag
      sub<-left_join(sub1[,c("date","true_rt")],sub2[,c("date",meteorology[i])], by = c("date")) %>% as.data.frame()
      cor<-cor.test(y=sub$true_rt,x=sub[,meteorology[i]],method="pearson")$estimate
      res_lag_tmp<-data.frame(pearson=cor,lag=lag,var=meteorology[i])
      res_lag<-bind_rows(res_lag,res_lag_tmp)
    }
  }  
  
  best_lag<-res_lag %>% 
    group_by(var) %>% 
    dplyr::slice(which.max(abs(pearson))) %>% 
    as.data.frame()
  
  return(best_lag)
}




obtain_data_afterlag = function(best_lag, data_raw){
  mypredictors =  c(stringency,meteorology)
  dt_update = data_raw[,c("date","true_rt","temp_rt")]
  for (i in 1: length(mypredictors)){
    dt_var<-data_raw[,c("date",mypredictors[i])]
    dt_var2 = dt_var
    dt_var2$date<-dt_var$date +  best_lag$lag[best_lag$var == mypredictors[i]]
    dt_update<-left_join(dt_update,dt_var2[,c("date",mypredictors[i])],by=c("date"))
  }
  other_var_final = dt_update %>% drop_na()
  
  return(other_var_final)
}
