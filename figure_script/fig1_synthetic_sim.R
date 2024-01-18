library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)
library(latex2exp)
library(ggpubr)

load("./simulation_synthetic/data/mysim.Rdata")  


##-------------------------------------------------Panel A
library(scales)
fold = 100
pA = dt_case %>% 
  filter(time<=100) %>% 
  ggplot(aes(x= time)) +
  geom_line(aes(y = true_case,color = "Infections"),size = 1) +
  geom_line(aes(y = report_shift_case, color = "Report-shift"),size = 1)+
  geom_line(aes(y = deconv_case, color = "Deconv-RIDE"),size = 1)+
  geom_line(aes(y = BP_case, color = "Deconv-BP"),size = 1)+
  
  geom_line(aes(y = true_rt*fold+300,),data = true_rt,size = 1,color = "black")+
  geom_line(aes(y = rt*fold+300,color = "Infections"),data = cori_real_incidence_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr*fold+300, ymax=rt_upr*fold+300,fill = "Infections"), alpha=0.1,data = cori_real_incidence_rt) +
  
  geom_line(aes(y = rt*fold+300,color = "Report-shift"),data = cori_reportshift_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr*fold+300, ymax=rt_upr*fold+300,fill = "Report-shift"), alpha=0.1,data = cori_reportshift_rt) +
  
  geom_line(aes(y = rt*fold+300,color = "Deconv-RIDE"),data = cori_RIDE_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr*fold+300, ymax=rt_upr*fold+300,fill = "Deconv-RIDE"), alpha=0.1,data = cori_RIDE_rt) +
  
  geom_line(aes(y = rt*fold+300,color = "Deconv-BP"),data = cori_BP_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr*fold+300, ymax=rt_upr*fold+300,fill = "Deconv-BP"), alpha=0.1,data = cori_BP_rt) +
  
  geom_vline(xintercept = sum(!is.na(dt_case$report_shift_case)),linetype = "dashed",color = "#a39193", size = 0.6) +
  geom_vline(xintercept = 100,linetype = "dashed",color = "#a39193", size = 0.6) +
  
  scale_color_manual(name = "Type", values = colors)+
  scale_fill_manual(name = "Type", values = colors)+
  scale_x_continuous(breaks = c(0,2,4,6,8,10)*10, expand = c(0.05, 0))+
  geom_hline(yintercept = fold +300, linetype="dashed", color = "#a39193", size=0.6)+
  #geom_vline(xintercept = 50, linetype="dashed", color = "#a39193", size=0.8)+
  geom_text(aes(x= 95,y = 630),label ="lag", col = "#84C3b7") +
  geom_segment(aes(x =  sum(!is.na(dt_case$report_shift_case)), xend= mt-1, 
                   yend= 600, y= 600),
               linewidth = 0.8,
               arrow = arrow(length = unit(0.02, "npc"),ends = "both"),col =  "#84C3b7") +
  theme_classic()+
  labs(
    title = " ",
    y = "Case",
    x = "Time"
  )+
  scale_y_continuous(  
    limits = c(0,650), breaks = c(0,1,2,3)*fold +300, labels = c(0,1,2,3), oob = rescale_none,
    "Effective reproductive number", #breaks = c(),
    sec.axis = sec_axis(~ ., name = 'case',breaks = c(0,100,200,300)),
  )+
  theme(
    # legend.position = c(0.2, 0.9),
    # legend.key.size = unit(0.5, 'cm'), #change legend key size
    legend.position = "right",
    axis.text = element_text(size = 12),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.title.y = element_text(vjust = 5,size = 12,hjust = 0.9),
    axis.title.y.right = element_text(margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 10)),
    legend.title=element_text(size = 10),
    legend.text =element_text(size = 10),
    plot.margin =  ggplot2::margin(20,20,20,20,unit = "pt")
  )
pA



##-------------------------------------------------Panel B and C

load("./simulation_synthetic/data/mysim_censored.Rdata")  


colors2 = c("Infections" = "#E68B81", "Report" =  "#8481BA" , "Deconv-RIDE (data up to time 100)" = "#7DA6C6", "Deconv-BP" = "#FBD178" , #"#f0d580"
            "Report-shift" =  "#84C3b7", "Deconv-RIDE (data up to time 40)" = "#8481BA" ,"Deconv-RIDE (data up to time 80)" = "#90b767")

true_rt = data.frame(time = 1:(mt-1),true_rt = rtdf$true_rt[1:(mt-1)])
library(scales)
pB = dt_case %>% 
  filter(time<=mt-1) %>% 
  ggplot(aes(x= time)) +
  #geom_line(aes(y = true_rt,),data = true_rt,size = 1,color = "black")+
  geom_line(aes(y = rt,color = "Infections"),data = cori_real_incidence_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Infections"), alpha=0.1,data = cori_real_incidence_rt) +
  
  geom_line(aes(y = rt,color = "Deconv-RIDE (data up to time 100)"),data = cori_RIDE_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Deconv-RIDE (data up to time 100)"), alpha=0.1,data = cori_RIDE_rt) +
  
  
  geom_line(aes(y = rt,color = "Deconv-RIDE (data up to time 40)"),data = cori_RIDE_rt_40,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Deconv-RIDE (data up to time 40)"), alpha=0.1,data = cori_RIDE_rt_40) +
  
  scale_color_manual(name = "Type", values = colors2)+
  scale_fill_manual(name = "Type", values = colors2)+
  scale_x_continuous(limits = c(20,40))+
  scale_y_continuous(limits = c(0,2.5))+
  
  geom_segment(aes(x = 40, xend= 40, 
                   yend= cori_RIDE_rt$rt[cori_RIDE_rt$time==40],
                   y= cori_RIDE_rt_40$rt[cori_RIDE_rt_40$time==40]),
               linewidth = 1,
               arrow = arrow(length = unit(0.03, "npc")))+
  
  theme_classic()+
  labs(
    title = " ",
    y = "Effective reproductive number",
    x = "Time"
  )+
  theme(
    legend.position = c(0.7, 0.95),
    legend.key.size = unit(0.5, 'cm'), #change legend key size
    #legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.title.y = element_text(vjust = 5,size = 12),
    legend.title=element_text(size = 10),
    legend.text =element_text(size = 10),
    plot.margin =  ggplot2::margin(20,20,20,20,unit = "pt")
  )
pB


pC = dt_case %>%  
  ggplot(aes(x= time)) +
  #geom_line(aes(y = true_rt,),data = true_rt,size = 1,color = "black")+
  geom_line(aes(y = rt,color = "Infections"),data = cori_real_incidence_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Infections"), alpha=0.1,data = cori_real_incidence_rt) +
  
  geom_line(aes(y = rt,color = "Deconv-RIDE (data up to time 100)"),data = cori_RIDE_rt,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Deconv-RIDE (data up to time 100)"), alpha=0.1,data = cori_RIDE_rt) +
  geom_line(aes(y = rt,color = "Deconv-RIDE (data up to time 80)"),data = cori_RIDE_rt_80,size = 1)+
  geom_ribbon(aes(ymin=rt_lwr, ymax=rt_upr,fill = "Deconv-RIDE (data up to time 80)"), alpha=0.1,data = cori_RIDE_rt_80) +
  
  scale_color_manual(name = "Type", values = colors2)+
  scale_fill_manual(name = "Type", values = colors2)+
  scale_x_continuous(limits = c(60,80))+
  scale_y_continuous(limits = c(0,2.5))+
  
  geom_segment(aes(x = 80, xend= 80, 
                   yend= cori_RIDE_rt$rt[cori_RIDE_rt$time==80],
                   y= cori_RIDE_rt_80$rt[cori_RIDE_rt_80$time==80]),
               linewidth = 1,
               arrow = arrow(length = unit(0.03, "npc")))+
  
  theme_classic()+
  labs(
    title = " ",
    y = "Effective reproductive number",
    x = "Time"
  )+
  theme(
    legend.position = c(0.7, 0.95),
    legend.key.size = unit(0.5, 'cm'), #change legend key size
    #legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title.x = element_text(vjust = -2, size = 12),
    axis.title.y = element_text(vjust = 5,size = 12),
    legend.title=element_text(size = 10),
    legend.text =element_text(size = 10),
    plot.margin =  ggplot2::margin(20,20,20,20,unit = "pt")
  )
pC





##-----------------------------------------Combine figure 
combined_plot1 = ggarrange(pA,
                           labels = c("A"), nrow = 1,
                           font.label = list(size = 11)) +
  theme(plot.margin = ggplot2::margin(10,15,5,15,unit = "pt"))


combined_plot = ggarrange(pB,pC,
                          labels = c("B","C"),
                          ncol = 2, nrow = 1,
                          font.label = list(size = 11)) +
  theme(plot.margin = ggplot2::margin(10,15,5,15,unit = "pt"))
# Add annotation lines of characters
annotation_text1<- textGrob(expression(bold(underline("STEP 1: Deconvolution"))), x = 0.03, y = 0.5, just = c("left", "top"), gp = gpar(fontsize = 11))

annotation_text2<- textGrob(expression(paste("Back-calculate the time series of infections from report cases and estimated ",
                                             "R"["t"] ," based on Cori’s method", sep = " ")), x = 0.03, y = 0.5, just = c("left", "top"), gp = gpar(fontsize = 11))

annotation_text3 <-  textGrob(expression(bold(underline("STEP 2: Correction"))), x = 0.03, y = 0.5, just = c("left", "top"), gp = gpar(fontsize = 11))

annotation_text4 <- textGrob(expression(paste("Incorporate other data sources to reduce the possible gap between ", 
                                              "R"["t"]^"k" , " (estimated from deconvolution time series of infections with data")),
                             x = 0.03, y = 0.5, just = c("left", "top"), gp = gpar(fontsize = 11))
annotation_text5 <- textGrob(expression(paste("up to time k) and ","R"["t"] , " (estimated from data of whole study period)")),
                             x = 0.03, y = 0.5, just = c("left", "top"), gp = gpar(fontsize = 11))

# Add the annotation to the combined plot
combined_plot2 <- arrangeGrob(annotation_text1,annotation_text2, combined_plot1, 
                               annotation_text3,annotation_text4, annotation_text5,combined_plot, 
                               heights = c(0.2,0.2,4,0.2,0.2,0.2,4))
grid.newpage()
grid.draw(combined_plot2)

ggsave(file="figure1.pdf", combined_plot2,width = 10,height = 8)
