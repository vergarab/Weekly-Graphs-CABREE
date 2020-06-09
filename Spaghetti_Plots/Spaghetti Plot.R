library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(scales)

taber_vols1<-read.csv("C:/Users/vergarab/Documents/RESEARCH/OFFICIAL FOR LOSS FUNCTION/TAB1/Brooks_Taber.csv",header = TRUE,
  na.strings = 999.99, stringsAsFactors = F, col.names = c("date","brooks_vols","brooks_vols_sim"))

taber_vols1$brooks_vols<-as.numeric(taber_vols1$brooks_vols)

taber_vols1$date<-ymd_hm(taber_vols1$date, tz="MST")
 
taber_vols<-taber_vols1%>% mutate(hour_start=hour(date),month=factor(month.name[month(date)],levels = month.name),
                                  year=year(date),day=as.Date(date))%>% group_by(hour_start, month, year, day) %>% 
   summarise(brooks_vols=mean(brooks_vols, na.rm = T))%>% filter(!is.na(brooks_vols))%>% filter(year==2009)
 
taber_vols2<-taber_vols1%>% mutate(hour_start=hour(date),month=factor(month.name[month(date)],levels = month.name),
                                   year=year(date),day=as.Date(date))%>% group_by(hour_start, month, year, day) %>% 
   summarise(brooks_vols=mean(brooks_vols_sim, na.rm = T)) %>% filter(year==2009)
 
 graph_data2<-taber_vols2 %>% group_by(month,hour_start)%>% 
   summarize(min_vol=min(brooks_vols),max_vol=max(brooks_vols),mean_vol=mean(brooks_vols),
             sd_vol=sd(brooks_vols) )
 
 
 taber_vols2<-na.omit(taber_vols2)

 graph_data<-taber_vols %>% group_by(month,hour_start)%>% 
   summarize(min_vol=min(brooks_vols),max_vol=max(brooks_vols),mean_vol=mean(brooks_vols),
             sd_vol=sd(brooks_vols) )
 start_date<-format(min(taber_vols$date),format = "%b %d, %Y")
 end_date<-format(max(taber_vols$date),format = "%b %d, %Y")
 target_months<-month.name

 
 ggplot() +
   geom_line(data=filter(taber_vols2, month %in% target_months), aes(hour_start,brooks_vols/81, group=day,colour="Daily patterns (Simulated)"),
             size=0.025)+
   geom_line(data=filter(taber_vols,month %in% target_months),aes(hour_start,brooks_vols/81,group=day,colour="Daily patterns (Market)"),
             size=.025)+
   geom_line(data=graph_data2 ,size=1.5, aes(hour_start, mean_vol/81, color= "Monthly mean pattern (Simulated)"))+
   geom_line(data=graph_data,size=1.5,aes(hour_start,mean_vol/81,color="Monthly mean pattern (Market)"))+
   facet_wrap(~month,ncol = 4)+
   theme( text=element_text(size=18, colour = "Black"), 
           plot.title = element_text(hjust = 0.5, face = "bold",color = "black"),
           plot.subtitle = element_text(hjust = 0.5, color="black"), 
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
           panel.background = element_rect(fill = "transparent"),
           panel.border = element_rect(colour="black", fill=NA, size=1))+
   scale_x_continuous(name="Hour",breaks = c(0,6,12,18,23),expand=c(0,0))+
   scale_y_continuous(name="Hourly Capacity Factor",breaks=pretty_breaks(),limits = c(0,1.05),labels=percent, expand = c(0,0))+
   scale_colour_manual("",values = c("Daily patterns (Market)"="red","Monthly mean pattern (Market)"="red3",
                                     "Daily patterns (Simulated)"="dodgerblue","Monthly mean pattern (Simulated)"="blue"))+
   guides(color=guide_legend(nrow=2, ncol=2, byrow = TRUE))
 
 
 ggplot(filter(graph_data,month %in% target_months)) +
   #geom_ribbon(aes(x = hour_start, ymin = min_vol, ymax = max_vol,fill="A"),alpha=.25, linetype = 0)+
   
   geom_line(data=filter(taber_vols,month %in% target_months),aes(hour_start,brooks_vols/81,group=day,colour="Daily patterns"),
             size=.05)+
   geom_line(size=1.5,aes(hour_start,mean_vol/81,color="Monthly mean pattern"))+
   facet_wrap(~month,ncol = 4)+
   theme( text=element_text(size=18, colour = "Black"), 
          plot.title = element_text(hjust = 0.5, face = "bold",color = "black"),
          plot.subtitle = element_text(hjust = 0.5, color="black"), 
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
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(colour="black", fill=NA, size=1))+
   scale_x_continuous(name="Hour",breaks = c(0,6,12,18,23),expand=c(0,0))+
   scale_y_continuous(name="Hourly Capacity Factor",breaks=pretty_breaks(),limits = c(0,1.05),labels=percent, expand = c(0,0))+
   scale_colour_manual("",values = c("grey30","red"))
 ggsave("brooks_ajl.png",width = 16,height = 10)
 
 
 scale_x_continuous(name="Hour", breaks = c(0,6,12,18,24),expand=c(0,0))+
   scale_y_continuous(name="Power Output (MW)", breaks = c(0,5,10,15,20,25,30,35),expand=c(0,0))+
 theme(text=element_text(size=16, colour = "Black"), 
       plot.title = element_text(hjust = 0.5, face = "bold",color = "black"),
       plot.subtitle = element_text(hjust = 0.5, color="black"), 
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
       panel.background = element_rect(fill = "transparent"),
       panel.border = element_rect(colour="black", fill=NA, size=1))+

