library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)

Aurora<-read.csv("C:/Users/vergarab/Documents/RESEARCH/AURORA/Data/Nat_ResourceHour.csv",header = TRUE,
                      na.strings = 999.99, stringsAsFactors = F)
Aurora$date<-ymd_h(Aurora$Time_Period, tz="MST")
Aurora$Output<-as.numeric(Aurora$Output)
Aurora$Capacity<-as.numeric(Aurora$Capacity)

Aurora<-Aurora%>% mutate(hour_start=hour(date),month=factor(month.name[month(date)],levels = month.name),
                                  year=year(date),day=day(date))

AuroraMod<-Aurora %>% filter(ID != "ANRN 589" , ID != "ANRN 605" , ID!="ANRN 606" , ID!="ANRN 607", ID!="ANRN 608" ) %>% group_by(hour_start, month, year, ID) %>% filter(year==2025) %>% summarise(Output=mean(Output, na.rm = T),
            Capacity=mean(Capacity, na.rm = T), Capacity_Factors=mean(Capacity_Factor,na.rm = T), CF=Output/Capacity)

Aurorayear<-Aurora %>% filter(ID != "ANRN 589" , ID != "ANRN 605" , ID!="ANRN 606" , ID!="ANRN 607", ID!="ANRN 608", ID!="TWF1", ID!="TWF10",
                              ID!="TWF11" , ID!="TWF12", ID!="TWF15", ID!="TWF16", ID!="TWF13", ID!="TWF18", ID!="TWF17",
                              ID!="TWF9", ID!="TWF20", ID!="TWF2", ID!="TWF19", ID!="TWF3", ID!="TWF21",
                              ID!="TWF14",ID!="TWF6") %>% group_by(hour_start, year, ID) %>% filter(year==2025) %>% summarise(Output=mean(Output, na.rm = T),
                 Capacity=mean(Capacity, na.rm = T), Capacity_Factors=mean(Capacity_Factor,na.rm = T), CF=Output/Capacity)
AuroraMod<-AuroraMod[-c(201457),] 


ggplot(data=AuroraMod, aes(x=hour_start, y=Capacity_Factors))+
  geom_line(aes(color=ID))+
  facet_wrap(~month, nrow = 3)+
  labs(title="Monthly Capacity Factors for Simulated Wind Farms",x="Hour", y="Capacity Factor (%)",
       subtitle = "Simulation for 2025, using 2009 CWA Data", caption = "Source: Aurora LTCE Simulated Output \n Graph by Natalia Vergara" )+
  theme(text=element_text(size=18, colour = "Black"),
        plot.title = element_text(hjust = 0.5, face = "bold",color = "Black"),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(size = 14, face = "italic"),
        axis.title.x = element_text(colour = "black"), 
        axis.title.y = element_text(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.line = element_line("black"),
        panel.border = element_rect(fill=NA,color = "black"),
        panel.spacing = unit(2, "lines"),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(labels=function(y) scales::percent(y, accuracy=1),expand=c(0,0))+
  scale_x_continuous(breaks = c(0,6,12,18,23), limits = c(0,24), expand=c(0,0))+
  guides(fill=guide_legend(override.aes=list()))


require(scales)
  
ggplot(data=Aurorayear, aes(x=hour_start, y=Capacity_Factors))+
  geom_line(aes(color=ID))+
  geom_point(aes(shape=ID, color=ID))+
  labs(title="Capacity Factor for Simulated Wind Farms",x="Hour", y="Capacity Factor (%)",
       subtitle = "Simulation for 2025, using 2009 CWA Data", caption = "Source: Aurora LTCE Simulated Output \n Graph by Natalia Vergara" )+
  theme(text=element_text(size=18, colour = "Black"),
        plot.title = element_text(hjust = 0.5, face = "bold",color = "Black"),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(size = 14, face = "italic"),
        axis.title.x = element_text(colour = "black"), 
        axis.title.y = element_text(colour = "black"),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.line = element_line("black"),
        panel.border = element_rect(fill=NA,color = "black"),
        panel.spacing = unit(2, "lines"),
        legend.position = "right",
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(labels=function(y) scales::percent(y, accuracy=1),expand=c(0,0), limits = c(0,0.8))+
  scale_x_continuous(breaks = c(0,6,12,18,23), limits = c(0,23), expand=c(0,0))+
  guides(fill=guide_legend(override.aes=list()))
