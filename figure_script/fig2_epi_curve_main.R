library(tidyverse)
library(forecast)
library(rlist)
library(randomForest)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(lubridate)
library(zoo)
library(scales)
library(latex2exp)
file.sources = list.files( c("./functions","./models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

cols = c("TRUE" = "Salmon",
         "RIDE" = "#90be6d",
         "BP" = "#dda15e",
         "ARIMA" = "#03045e")

lines = c("TRUE" = "solid",
          "RIDE" = "twodash",
          "BP" = "twodash",
          "ARIMA"  = "solid")

lines_size =  c("TRUE" = 0.8,
                "RIDE" = 0.8,
                "BP" = 0.8,
                "ARIMA"  = 0.8)

case_df  = read_rds("./data/data_rt_cov_all.rds") %>% 
  dplyr::select(date,confirm) %>% 
  as.data.frame()
true_rt_all = read_rds("./data/data_rt_cov_all.rds")  %>%  
  dplyr::select(date,true_rt) %>% as.data.frame()



curve_plot = function(myperiod){
  mywave_title = ifelse(myperiod == "train","Wave 5",ifelse(myperiod == "test","Wave 6","Wave 4"))
  
  arima_all = combine_data("arima") %>% 
    filter(period == myperiod) %>% 
    filter(proj == 0) 
  
  df_raw = arima_all
  df_raw$ensemble_rt = 0
  df2= left_join(true_rt_all,df_raw %>% dplyr::select(-true_rt),by = "date")
  
  
  
  period_date = data.frame(period = c("train","test","valid"),
                           start_date = c("2022-02-15","2022-07-01","2021-01-01"),
                           end_date = c("2022-06-30","2022-12-31","2021-03-31"))
  
  seq_date = seq(as.Date(period_date$start_date[period_date$period==myperiod]),
                 as.Date(period_date$end_date[period_date$period==myperiod]), by = "day")
  
  
  df =  df2 %>% filter(date%in%seq_date)
  df_long  = 
    df %>% 
    dplyr::select(date,true_rt,pred_rt,rt_temp_RIDE, rt_temp_BP, rt_temp_epinow2,ensemble_rt) %>% 
    rename("TRUE" = "true_rt",
           "RIDE" = "rt_temp_RIDE",
           "ARIMA" = "pred_rt",
           "EpiNow2" = "rt_temp_epinow2",
           "BP" = "rt_temp_BP",
           "ENSEMBLE" = "ensemble_rt"
    ) %>% 
    pivot_longer(
      2:7,
      values_to = "Rt",
      names_to = "model"
    ) %>% 
    mutate(
      model = sub("\\_Const.*", "", model),
      model = factor(model, levels = c("TRUE","ARIMA","ENSEMBLE","RIDE","BP"))
    ) %>% 
    filter(model!= "ENSEMBLE")
  
  if(myperiod == "train"){
    fold = 20000
  }else if(myperiod == "test"){
    fold = 2000
  }else if(myperiod == "valid"){
    fold = 40
  }else{
    fold = 50
  }
  
  
  period_title = ifelse(myperiod == "valid","testing period of ancestral strain",ifelse(myperiod == "test","testing period of Omicron","training period"))
  year = substr(df_long$date[1],1,4)
  case_df_period = case_df %>% filter(date%in%seq_date )
  if(myperiod != "train"){
    p = 
      df%>% 
      ggplot()+
      geom_bar(aes(x = date, y = confirm),stat = "identity", fill ='#e2dceb',alpha = 0.8,data = case_df_period)+
      geom_hline(yintercept = 1*fold, linetype="dashed", color = "#a39193", size=0.6)+
      geom_line(aes(x = date, y = Rt*fold, group = model,
                    size = model, color = model,linetype = model),
                data = df_long %>% filter(model!= "ENSEMBLE"  & model!= "TRUE"))+
      geom_line(aes(x = date, y = Rt*fold), color = "Salmon", size = 0.8,
                data = df_long %>% filter(model== "TRUE"))+
      scale_colour_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values = cols)+
      scale_linetype_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values = lines)+
      scale_size_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values=lines_size)+
      theme_bw() +
      labs(
        title = c(paste(mywave_title, " (",period_title,")",sep = "")),
        y = "Rt",
        x = year
      )+
      scale_y_continuous(  
        limits = c(0,3*fold), breaks = c(1,2,3)*fold, labels = c(1,2,3), oob = rescale_none,
        "Effective reproductive number", #breaks = c(),
        sec.axis = sec_axis(~ ., name = 'case',),
      )+
      scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
      theme( strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
             legend.position = "bottom",
             strip.text = element_text(size = 9),
             axis.text = element_text(size = 8),
             axis.title.x = element_text(vjust = -1, size = 9),
             axis.title.y = element_text(vjust = 2, size = 9),
             legend.title=element_text(size = 9),
             legend.text =element_text(size = 8),
             legend.spacing.x = unit(1.0, 'cm'),
             plot.title = element_text(size = 9),
             plot.margin = ggplot2::margin(5,15,10,15,unit = "pt")
      )
  }else{
    p = 
      df%>% 
      ggplot()+
      geom_bar(aes(x = date, y = confirm),stat = "identity", fill ='#e2dceb',alpha = 0.8,data = case_df_period)+
      geom_hline(yintercept = 1*fold, linetype="dashed", color = "#a39193", size=0.6)+
      geom_vline(xintercept = as.Date("2022-03-01"), linetype="dashed", color = "#a39193", size=0.6)+
      geom_line(aes(x = date, y = Rt*fold, group = model,
                    size = model, color = model,linetype = model),
                data = df_long %>% filter(model!= "ENSEMBLE" & model != "TRUE"))+
      geom_line(aes(x = date, y = Rt*fold), color = "Salmon", size = 0.8,
                data = df_long %>% filter(model== "TRUE"))+
      scale_colour_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values = cols)+
      scale_linetype_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values = lines)+
      scale_size_manual(name = TeX(paste("Nowcast of","$R_{t}$","$(R_{t}^t)$",sep = " ")),values=lines_size)+
      theme_bw() +
      labs(
        title = c(paste(mywave_title, " (",period_title,")",sep = "")),
        y = "Rt",
        x = year
      )+
      scale_y_continuous(  
        limits = c(0,3.5*fold), breaks = c(1,2,3)*fold, labels = c(1,2,3), oob = rescale_none,
        "Effective reproductive number", #breaks = c(),
        sec.axis = sec_axis(~ ., name = 'case',),
      )+
      scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
      theme( strip.background =element_rect(fill=ggplot2::alpha("darkblue",0.1)),
             legend.position = "bottom",
             strip.text = element_text(size = 9),
             axis.text = element_text(size = 8),
             axis.title.x = element_text(vjust = -1, size = 9),
             axis.title.y = element_text(vjust = 2, size = 9),
             legend.title=element_text(size = 9),
             legend.text =element_text(size = 8),
             legend.spacing.x = unit(1.0, 'cm'),
             plot.title = element_text(size = 9),
             plot.margin = ggplot2::margin(5,5,10,15,unit = "pt")
      )
    
  }
  return(p)
}

A = curve_plot("train")
B = curve_plot("test")
C = curve_plot("valid")
curve_p = ggarrange(C,A,B,
                    labels = c("A","B","C"),
                    ncol = 1, nrow = 3,
                    common.legend = TRUE, legend="bottom",
                    font.label = list(size = 12)) +
  theme(plot.margin = ggplot2::margin(10,15,0,15,unit = "pt"))


dt = data.frame(x = 1:10, y = 1:10, type = "TRUE")
p_true = dt %>% ggplot(aes(x,y,col = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(
    legend.position = "bottom", legend.title=element_text(size = 9),
    legend.text =element_text(size = 8),
    legend.spacing.x = unit(1.0, 'cm'),
    plot.margin = ggplot2::margin(0,5,15,5,unit = "pt"))+
  scale_color_manual(name = TeX("$R_{t}$"),values = "Salmon")
leg <- get_legend(p_true)

ppp = cowplot::plot_grid(curve_p,NULL, leg,
  ncol = 1, rel_heights = c(15,-0.2, 0.6)) 

ppp
ggpubr::ggexport(ppp,filename = "./figure_result/figure2.pdf",width=10, height=8)
#ggsave(ppp,filename = "./figure_result/fig2.tiff", width = 10, height = 8, device='tiff', dpi=700)






