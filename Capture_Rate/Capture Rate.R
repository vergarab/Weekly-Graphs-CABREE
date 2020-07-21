#Capture Rate Calculation 
#By: Natalia Vergara 
#Last Modify: 2020-07-20
#_________________________________________________________________

#Libraries (To install libraries use install.packages())
{library(tidyverse)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(formattable)}

#Data Import:
Resources<-read.csv("C:/Users/vergarab/Documents/RESEARCH/AURORA/Data/ResourceGroupYear.csv",header = TRUE,
              na.strings = 999.99, stringsAsFactors = F)

Pool<-read.csv("C:/Users/vergarab/Documents/RESEARCH/AURORA/Data/ZoneYear_Average_Year_Alberta.csv",header = TRUE,
                    na.strings = 999.99, stringsAsFactors = F)

#Energy revenue 
Resources$Resource_Energy_Revenue<-Resources$Energy_Revenue*1000

#Capture Rate 
Resources$Capture_Rate<-(Resources$Resource_Energy_Revenue/Resources$Pool_Total_Revenue)*100

Resources_base<-Resources %>% filter(ID!="MRO", ID!="AB",ID!="AB_Renewables", ID!="Intertie",Run_ID=="Base Case")
Resources_forced<-Resources %>% filter(ID!="MRO", ID!="AB",ID!="AB_Renewables", ID!="Intertie",Run_ID=="Forcing Wind")


ggplot()+
  geom_bar(data=Resources_base, aes(x=Time_Period, y=Capture_Rate, group=ID, fill=ID), stat = "identity", 
           position = "dodge")+
  geom_bar(data=Resources_forced, aes(x=Time_Period, y=Capture_Rate, group=ID, fill=ID), stat = "identity", 
           position = "dodge")+
  labs(x="Year", y="Capture Rate (%)", caption = "Source: CABREE's Aurora Model Output \n Graph by Natalia Vergara")+
  facet_wrap(~Run_ID, ncol = 2)+
  theme_bw()+
  theme(text=element_text(size=18, colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold",color = "black"),
        plot.subtitle = element_text(hjust = 0.5, color="black"), 
        plot.margin = margin(1,1,1,1,"cm"),
        axis.title.x = element_text(colour = "black"), 
        axis.title.y = element_text(colour = "black"),
        axis.line = element_line("black"),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        panel.spacing = unit(1.5, "lines"), 
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black", fill=NA, size=1)
       )+
  scale_x_continuous(name="Year", breaks = c(2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030),expand=c(0,0), limits = c(2019,2031))+
  scale_y_continuous(name="Capture Rate (%)",breaks = c(0,10,20,30,40,50,60,70), limits = c(0,71),expand=c(0,0))+
  scale_fill_viridis_d()+
  guides(fill=guide_legend(override.aes=list()))
  


