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

all_df = read_rds("./tidy_result/tidy_res/all_coef.rds")




coef_ci_all_plot = function(all_df,mywave){
  mywave_title = ifelse(mywave == "Omicron","Omicron variant","ancestral strain")
  #mycolor  = ifelse(mymodel == "#00BFC4","#F8766D")
  df = all_df %>% filter(wave == mywave) %>% 
    mutate(ifelse(vars == "temp_rt","rt_temp_past_0",vars))
  
  
  cols = c('ARIMA'="#F8766D", 'ARIMA'="#00BFC4")
  var_final = unique(df$vars) %>% sort()
  x = var_final
  i1 = match(x,c(paste("rt_temp_past_",0:14,sep = ""),"humidity","temperature","wind_speed", 
                 "ContainmentHealthIndex_WeightedAverage","EconomicSupportIndex","GovernmentResponseIndex_WeightedAverage","StringencyIndex_WeightedAverage"))
  #df$vars = factor(x,levels = x[order(i1,decreasing = T)])
  #a = transform_text(df$vars)[order(i1,decreasing = T)]
  var_final = var_final[order(i1,decreasing = T)]
  
  p= list()
  for( k in c(1:length(var_final))){
    var1 = var_final[k]
    if(var1 == "wind_speed"){
      a = "Wind Speed"
    }else if(var1 == "temperature"){
      a = "Temperature"
    }else if(var1 == "humidity"){
      a = "Relative Humidity"
    }else if(var1 == "StringencyIndex_WeightedAverage"){
      a = "Stringency Index" 
    }else if (var1 == "ContainmentHealthIndex_WeightedAverage"){
      a = "Containment Health Index" 
    }else if (var1 == "GovernmentResponseIndex_WeightedAverage"){
      a = "Government Reponse Index"
    }else if (var1 == "EconomicSupportIndex"){
      a = "Economic Support Index"
    }else if(str_detect(var1,"rt_temp")){
      var_date = str_split(var1, '_', simplify = TRUE)[,4] %>% as.numeric()
      a = paste('$R_{t-',var_date,'}^{t}$',sep = "")
    }else{
      a = var1
    }
    mywave_title = ifelse(mywave == "Omicron","Omicron variant","ancestral strain")
    var_p = 
      df %>% filter(vars == var1) %>% 
      ggplot(aes(x=as.Date(date), y=mean))+
      geom_pointrange(aes(ymin=lower,ymax=upper,color = model),size = 0.3)+
      scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
      theme_bw() +
      labs(
        title = unname(TeX(a)),
        x = 2022,
        y = "Coefficient"
      )+
      theme(
        strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
        title =element_text(size=9),
        axis.text= element_text(size = 9),
        axis.title.x = element_text(vjust = -1, size = 10),
        axis.title.y = element_text(vjust = 1, size = 10),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10),
        legend.box.margin=ggplot2::margin(10,10,10,10),
        legend.position = "none",
        plot.margin =  ggplot2::margin(15,15,15,15,unit = "pt"),
        panel.spacing = unit(1, "cm")
      )+
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+
      scale_color_manual(name='Model',values=cols)
    
    p[[k]] = var_p
  }
  return(p)
}


B = ggarrange(plotlist = coef_ci_all_plot(all_df,"Omicron"),nrow = 2,ncol = 3)
p_B = annotate_figure(B, top = text_grob("Coefficients of predictors with 95% confidence interval for Omicron wave", 
                                         face = "bold", size = 12))
ggpubr::ggexport(p_B,filename = "./figure_result/figure_S6.pdf",width=10, height=6)


