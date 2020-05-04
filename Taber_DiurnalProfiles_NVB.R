#This code creates - Diurnal Profiles-

library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)


RealSim<-read.csv("FIXEDFUNCTION_Taber_Blip.csv", stringsAsFactors = F)
RealSim<-FIXEDFUNCTION_Taber_BLIP

Monthly_Data<-RealSim %>% group_by(month,hour) %>%  summarise(Taber_Real=mean(EnemaxTaber,na.rm=T),Taber_Sin=mean(PO_85_SimFix, na.rm = T))
Monthly_Data$month=factor(Monthly_Data$month, levels=month.name)

{ggplot(data=Monthly_Data) + 
    geom_line(aes(x = hour, y = Taber_Sin, colour= 'Simulated Output'), colour="red")+
    geom_point(aes(x = hour, y = Taber_Sin, colour= 'Simulated Output', shape='Simulated Output'),size=1, colour="red")+
    geom_line(aes(x=hour, y=Taber_Real, colour='Market Data'), colour="black")+
    geom_point(aes(x=hour, y=Taber_Real, colour='Market Data', shape='Market Data'), size=1,colour="black")+
    facet_wrap(~ month, nrow = 3) + 
    labs(x="Hours", y="Power Output (MW)", title= "Mean Diurnal Profiles for Enmax Taber", subtitle= "Simulation data from 2008-2010" )+
    theme(text=element_text(size=18, colour = "Black"),
          plot.title = element_text(hjust = 0.5, face = "bold",color = "Black"),
          plot.subtitle = element_text(hjust = 0.5), 
          axis.title.x = element_text(colour = "black"),
          axis.title.y = element_text(colour = "black"),
          axis.line = element_line("black"),
          panel.spacing = unit(1.5, "lines"), 
          legend.position = "bottom",
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.length = unit(0.25, "cm"),
          axis.ticks.margin=unit(0.5, "cm"),
          panel.border = element_rect(colour="black", fill=NA, size=1))+
    scale_x_continuous(name="Hour", breaks = c(0,6,12,18,24))+
    scale_shape_manual("", values=c('Simulated Output'=1, 'Market Data'=2))+
    guides(fill=guide_legend(override.aes=list()))}

ggsave("Taber_DiurnalProfiles_NVB.png", width = 16, height = 10)
