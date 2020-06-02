library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)
library(tibble)
library(shadowtext)

df<-read.csv("C:/Users/vergarab/Documents/RESEARCH/3 Min Figure/Coal Phase-out/TotalGenerationFuel.csv",header = TRUE,
             na.strings = 999.99) 

df2<-df %>% filter(FuelType!= "Renewables"  & Year>=2000)


ggplot(data=df2, aes(x=Year, y=Generation, fill=FuelType, color=FuelType))+
  geom_line()+
  theme(text=element_text(size=14, colour = "Black"), 
        plot.title = element_text(hjust = 0.5, face = "bold",color = "black"),
        plot.subtitle = element_text(hjust = 0.5, color="black"), 
        axis.title.x = element_text(colour = "black"), 
        axis.title.y = element_text(colour = "black"),
        axis.line = element_line("black"),
        axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5),"cm")),
        panel.spacing = unit(1.5, "lines"), 
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(colour="black", fill=NA, size=1))+
  scale_fill_viridis_d()+
  scale_x_continuous(name="Year", breaks = c(0,2000,2005,2010,2015,2020))+
  scale_y_continuous(name="Generation (GWh)",breaks = c(10000,20000,30000,40000,50000))
  
