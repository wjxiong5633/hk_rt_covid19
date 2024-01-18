gam_fit = function(training.set,w){
  stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
                 "ContainmentHealthIndex_WeightedAverage","EconomicSupportIndex")
  meteorology<<- c("temperature","humidity","abs_humidity","wind_speed")
  vars =  colnames(training.set)[-c(1:2)]
  ind = vars %in% c(stringency)
  ns_var = vars[ind == T]
  ## Smoothing var
  s_var = vars[ind == F]
  
  if(nrow(training.set) >=80){
    c =  "s(temp_rt)"
    b = paste0("s(",s_var,",k =",round(nrow(training.set)/40),")",collapse="+")
  }else{
    c = "temp_rt"
    b = paste0(s_var,collapse="+")
  }
  
  if(length(ns_var)>=1 & length(s_var)>=1){
    a = paste0(ns_var,collapse="+")
    b =b
    f<-as.formula(paste0("true_rt~", paste(a,b,c,sep = " + ")))
  }else if(length(ns_var)==0 & length(s_var)>=1){
    a = NA
    b = b
    f<-as.formula(paste0("true_rt~ ", paste(b,c,sep = " + ")))
  }else if(length(s_var)==0 & length(ns_var)>=1){
    a = paste0(ns_var,collapse="+")
    b = NA
    f<-as.formula(paste0("true_rt~", paste(a,c,sep = " + ")))
  }else{
    f = NA
  }
  
  model = gam(f, data = training.set,method = "REML")
  return(model)
}
