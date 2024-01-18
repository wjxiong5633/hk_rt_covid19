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


date1 = c(seq(as.Date("2020-07-01"),as.Date("2020-08-31"), by="day"))
date2 = c(seq(as.Date("2020-12-01"),as.Date("2021-03-31"), by="day"))
date3 = c(seq(as.Date("2022-02-01"),as.Date("2022-12-31"), by="day"))

cov_df_raw = read_csv("./data/data_all_Dec.csv") %>% 
  mutate(date = as.Date(date))%>%
  filter(date >= "2020-06-01") %>% as.data.frame()
head(cov_df_raw)

cov_df_raw$time = ifelse(cov_df_raw$date%in%date1,"wave3",ifelse(cov_df_raw$date%in%date1, "wave4", "wave5"))                
cov_df_raw = cov_df_raw %>% filter(date >= "2020-11-01")

datebreaks3<-seq.Date(from = as.Date('2020-12-01'),to = as.Date("2022-10-31"),by="6 month")

rects<-data.frame(xstart=as.Date(c('2020-12-01','2022-02-01')),  #'2020-07-01',v
                  xend=as.Date(c('2021-03-31','2022-12-31')))  ##'2020-08-31'

meteorology = c("temperature","humidity","wind_speed")
p_metero = lapply(1:3,function(i){
  var1 = meteorology[i]
  if(var1 == "wind_speed"){
    a = "Wind speed"
  }else if(var1 == "temperature"){
    a = "Temperature(°C)"
  }else{
    a = "Relative humidity(%)"
  }
  var_p = 
    ggplot()+
    geom_line(data = cov_df_raw, aes_string(x = "date",y = var1,group = "time"),col='grey')+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend,ymin=-Inf,ymax=Inf),fill='#bcd4e6',alpha=0.4)+
    theme_bw() +
    labs(
      x = "Date",
      y = a,
      title = a
    )+
    theme(
      title =element_text(size=9),
      axis.text= element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.title.x = element_text(margin = ggplot2::margin(t =20, r = 0, b = 0, l = 0)),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.box.margin=ggplot2::margin(10,10,10,10),
      plot.margin =  ggplot2::margin(15,15,15,15,unit = "pt")
    )+
    scale_x_date(date_labels = "%Y/%m",breaks=datebreaks3)
  return(var_p)
})


metero_p = ggarrange(plotlist = p_metero,
                     labels = LETTERS[1:3],ncol = 2, nrow = 2,
                     font.label = list(size = 12)) +
  theme(plot.margin = ggplot2::margin(15,15,5,15,unit = "pt"))

ggpubr::ggexport(metero_p,filename = "./figure_result/figure_S7.pdf",width=10, height=8)



policy = c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage", 
           "ContainmentHealthIndex_WeightedAverage","EconomicSupportIndex")
p_policy = lapply(1:4, function(i){
  var1 = policy[i]
  if(var1 == "StringencyIndex_WeightedAverage"){
    a = "Stringency Index" 
  }else if (var1 == "ContainmentHealthIndex_WeightedAverage"){
    a = "Containment Health Index" 
  }else if (var1 == "GovernmentResponseIndex_WeightedAverage"){
    a = "Government Reponse Index"
  }else if (var1 == "EconomicSupportIndex"){
    a = "Economic Support Index"
  }
  var_p = 
    ggplot()+
    geom_line(data = cov_df_raw, aes_string(x = "date",y = var1),col='grey')+
    geom_rect(data=rects,aes(xmin=xstart,xmax=xend,ymin=-Inf,ymax=Inf),fill='#bcd4e6',alpha=0.4)+
    theme_bw() +
    labs(
      x = "Date",
      y = a,
      title = a
    )+
    theme(
      title =element_text(size=9),
      axis.text= element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.title.x = element_text(margin = ggplot2::margin(t =20, r = 0, b = 0, l = 0)),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.box.margin=ggplot2::margin(10,10,10,10),
      plot.margin =  ggplot2::margin(15,15,5,15,unit = "pt")
    )+
    scale_x_date(date_labels = "%Y/%m",breaks=datebreaks3)
  
  return(var_p)
})


policy_p = ggarrange(plotlist = p_policy,
                     labels = LETTERS[1:4],ncol = 2, nrow = 2,
                     font.label = list(size = 12)) +
  theme(plot.margin = ggplot2::margin(15,15,15,15,unit = "pt"))

ggpubr::ggexport(policy_p,filename = "./figure_result/figure_S8.pdf",width=10, height=8)
#ggpubr::ggexport(policy_p,filename = "./figure/policy.pdf",width=20, height=15)

