library(tidyverse)
library(caret)
library(forecast)
library(data.table)
library(rlist)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(pROC)
library(zoo)

file.sources = list.files( c("./functions","./models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)




## BP and RIDE
error_plot = function(mypred){
  model_pf = read.csv("./tidy_result/Omicron_others.csv")
  all_pf = model_pf %>% filter(modelname!= "ensemble")
  
  
  pf_long = all_pf %>% 
    pivot_longer(
      "rmse":"auc",
      names_to = "metrics",
      values_to = "value"
    ) %>% 
    mutate(
      metrics = factor(metrics,levels = c("rmse","mape","auc"),
                       labels = c("RMSE","MAPE","AUC")),
      period = factor(period, levels = c("train","test","valid"),labels = c("Training","Testing(Omicron)","Testing(Ancestral strain)")),  
      model = str_to_upper(modelname),
      model = ifelse(model == "SVR_L","SVR",model)
    )%>% 
    arrange(period,metrics,proj)

  if(mypred == "nowcast"){
    pf_long_proj = pf_long %>% filter(proj<= 0)
    breaks = c(-13:0)
  }else if(mypred == "forecast"){
    pf_long_proj = pf_long %>% filter(proj>0)
    breaks = c(1:7)
  }else{
    pf_long_proj = pf_long
    breaks = c(-13:7)
  }
  
  # pf_nowcast = pf_long %>% filter(proj <= 0)
  # pf_forecast = pf_long %>% filter(proj>0)
  
  ##  7d Avg plot --------------------------------------- 
  pf_avg = 
    pf_long_proj %>% 
    group_by(period,model,metrics) %>% 
    dplyr::summarise(
      avg_value = round(mean(value),3)) %>% 
    arrange(period,metrics) %>% 
    mutate(
      model = sub("\\_Const.*", "", model)
    )
  
  mywave_title = ifelse(mywave == "Omicron","Omicron variant","ancestral strain")
  pal<- c("Training" =  'goldenrod',
          "Testing(Omicron)" =  "brown1",
          "Testing(Ancestral strain)"= "dodgerblue4")
  
  
  unique_models = unique(pf_avg$model)
  
  #pf_avg$model = factor(pf_avg$model,levels = c(unique_models[-c(unique_models=="ARIMA")],"ARIMA"))
  face_vec = ifelse(unique_models %in% c("ENSEMBLE","ARIMA"),"bold","plain")
  size_vec = ifelse(unique_models%in% c("ENSEMBLE","ARIMA"),7,7)
  
  
  # col = 'goldenrod'
  
  
  avg_p = 
    pf_avg %>% 
    ggplot(aes(x = model,y = avg_value,label = avg_value))+ 
    geom_point(size =  1, aes(color = period)) + 
    scale_color_manual(values=pal,name = "Period")+
    theme_bw()+
    geom_text(
      aes(x = model, y = avg_value, label = avg_value),
      position = position_dodge2(padding = 0.2),
      vjust = -0.5, size = 2.5, hjust = 0.5,
    )+
    #geom_text_repel(size = 2, point.padding = 0.2,position = "dodge")+
    facet_grid(metrics~period,scales = "free")+
    labs(
      title = paste(str_to_title(mypred)," performance over study period ",sep = ""),
      x = 'Models',
      y = 'Value')+
    theme(axis.text.x = element_text(face = face_vec,size = size_vec),
          strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
          legend.position = "bottom",
          strip.text = element_text(size = 9),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(vjust = -1, size = 9),
          axis.title.y = element_text(vjust = 2, size = 9),
          legend.title=element_text(size = 9),
          legend.text =element_text(size = 8),
          legend.spacing.x = unit(0.3, 'cm'),
          plot.title = element_text(size = 10),
          plot.margin =  ggplot2::margin(5,10,5,10,unit = "pt")
    )+
    ggh4x::facetted_pos_scales(y = list(
      metrics == "RMSE" ~ scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6),labels = scales::number_format(auc = 0.1)),
      metrics == "MAPE" ~ scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6),labels = scales::number_format(auc = 0.1)),
      metrics == "AUC" ~ scale_y_continuous(limits = c(0.3,1.1), breaks = c(0.4,0.6,0.8,1),labels = scales::number_format(auc = 0.1))
    ))
  
  
    #scale_y_continuous(labels = scales::number_format(auc = 0.1))
  avg_p
  ##  7d proj plot --------------------------------------- 
  
  
  cols = c("ARIMA" = "#03045e",
           "GAM" = "#2a9d8f",
           "GBM" = "#dda15e",
           "GPR" = "#9a8c98", 
           "NN" = "#90be6d",
           "RF" = "#a0c4ff",            
           "RIDGE" = "#ffc8dd",
           "SVR" = "#dfa290",
           "ENSEMBLE" = "#7289da")
  
  lines = c("ARIMA" = "solid",
            "GAM" = "twodash",
            "GBM" = "twodash",
            "GPR" = "twodash", 
            "NN" = "twodash",
            "RF" = "twodash",
            "RIDGE" = "twodash",
            "SVR" = "twodash",
            "ENSEMBLE" = "solid")
  
  lines_size =  c("ARIMA" = 0.8,
                  "GAM" = 0.8,
                  "GBM" = 0.8,
                  "GPR" = 0.8,
                  "NN" = 0.8,
                  "RF" = 0.8,
                  "RIDGE" = 0.8,
                  "SVR" = 0.8,
                  "ENSEMBLE" = 0.8)
  
  # if(mypred == "nowcast"){
  #   pf_long_proj2 = pf_long_proj %>% filter(period != "Training") 
  # }else{
  #   pf_long_proj2 = pf_long_proj
  # }
  
  pf_long_proj2 = pf_long_proj %>% filter(period != "Training") 
  proj_p = 
    pf_long_proj2 %>% 
    ggplot(aes(x = proj,y = value,color = model))+
    geom_point(size =  1)+
    geom_line(aes(linetype = model,size = model))+
    scale_colour_manual(name = "Model",values = cols)+
    scale_linetype_manual(name = "Model",values = lines)+
    scale_size_manual(name = "Model",values=lines_size)+
    theme_bw()+
    facet_grid(metrics~period,scales = "free")+
    labs(
      title = paste(str_to_title(mypred), " performance of each predicition horizon" ,sep = ""),
      x = "Days ahead",
      y = 'Value'
    )+
    theme(strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
          legend.position = "bottom",
          strip.text = element_text(size = 9),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_text(vjust = -1, size = 9),
          axis.title.y = element_text(vjust = 2, size = 9),
          legend.title=element_text(size = 9),
          legend.text =element_text(size = 8),
          legend.spacing.x = unit(0.3, 'cm'),
          plot.title = element_text(size = 10),
          plot.margin =  ggplot2::margin(5,10,5,10,unit = "pt")
    )+
    scale_y_continuous(labels = scales::number_format(auc = 0.1))+
    scale_x_continuous(breaks = breaks)+
    ggh4x::facetted_pos_scales(y = list(
      metrics == "RMSE" ~ scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6),labels = scales::number_format(auc = 0.1)),
      metrics == "MAPE" ~ scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6),labels = scales::number_format(auc = 0.1)),
      metrics == "AUC" ~ scale_y_continuous(limits = c(0.3,1.1), breaks = c(0.4,0.6,0.8,1),labels = scales::number_format(auc = 0.1))
    ))
  
  
  proj_p
  
  return(list(avg_p, proj_p))
}

## Nowcast
A2 = error_plot("nowcast")

error_p =ggarrange(A2[[1]], A2[[2]], nrow = 2, labels = c("A", "B"), 
                   legend="bottom",font.label = list(size = 12)) +
  theme(plot.margin = ggplot2::margin(10,10,5,10,unit = "pt"))
ggpubr::ggexport(error_p,filename = "./figure_result/figure_S2.pdf",width=10, height=10)



## Forecast
B2 = error_plot("forecast")

error_p =ggarrange(B2[[1]], B2[[2]], nrow = 2, labels = c("A", "B"), 
                   legend="bottom",font.label = list(size = 12)) +
  theme(plot.margin = ggplot2::margin(10,10,5,10,unit = "pt"))
ggpubr::ggexport(error_p,filename = "./figure_result/figure_S3.pdf",width=10, height=10)

