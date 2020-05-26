
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(ggrepel)
library(tibble)
library(shadowtext)

df<-read.csv("C:/Users/vergarab/Documents/RESEARCH/3 Min Figure/PieChart/NetInstalledCapacity_1518.csv",header = TRUE,
             na.strings = 999.99) 
df<-df[,-c(3)]
df18<-read.csv("C:/Users/vergarab/Documents/RESEARCH/3 Min Figure/PieChart/NetInstalledCapacity_18.csv",header = TRUE,
             na.strings = 999.99) 

df$perc<-100*(df$Capacity/sum(df$Capacity))

df18$perc<-100*(df18$Capacity/sum(df18$Capacity))

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=18, face="bold")
  )


ggplot(df, aes(x="",y=perc, fill=Fuel))+
  geom_bar(width=1, stat = "identity")+
  coord_polar("y",start = 0)+
  geom_text_repel(aes(label=paste0(round(perc),"%")),
            position = position_stack(vjust = 0.5), color="darkgray",size=5)+
  scale_fill_viridis_d()+
  blank_theme+
  labs(title="Alberta's Net Installed Capacity in 2015", caption = "Data Source:  Data from Alberta Utilities Commission (AUC).")+
  theme(axis.text = element_blank())

ggplot(df18, aes(x="",y=perc, fill=Fuel))+
  geom_bar(width=1, stat = "identity")+
  coord_polar("y",start = 0)+
  geom_text_repel(aes(label=paste0(round(perc),"%")),
                  position = position_stack(vjust = 0.5), color="darkgray",size=5)+
  scale_fill_viridis_d()+
  blank_theme+
  labs(title="Alberta's Net Installed Capacity in 2018", caption = "Data Source:  Data from Alberta Utilities Commission (AUC).")+
  theme(axis.text = element_blank())
  
  
