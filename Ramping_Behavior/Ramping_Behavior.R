library(ggplot2)
library(gganimate)
library(tidyverse)
library(gapminder)
library(gifski)
library(png)


Data<-read.csv("C:/Users/vergarab/Documents/RESEARCH/OFFICIAL FOR LOSS FUNCTION/TAB1/FIXEDFUNCTION_Taber_BLIP.csv", stringsAsFactors = F)

Data<- Data %>% filter(year==2009, month== "October", day<=13, day>=5) %>% 
  group_by(day,hour) %>% summarise(Real=mean(EnemaxTaber, na.rm = T), Sim=mean(PO_85_SimFix, na.rm = T)) 

Df<-data.frame(Hour=rep(Data$hour,2), Day=rep(Data$day,2), sim_real=c(Data$Real, Data$Sim),
               grp=rep(c("Sim", "Real"), each=nrow(Data)), Hour_Week=seq.int(nrow(Data)))

p<-ggplot(Df, aes(x=Hour_Week, y=sim_real, group=grp, color=factor(grp)))+
  geom_line()+
  labs(x="Hour", y= "Power Output (MW)")+
  theme_bw()+
  theme(legend.position = "botom")

g<-p+geom_point()+transition_time(Hour_Week)+transition_reveal(Hour_Week)+
  labs(title="Enmax Taber", subtitle = "From October 5th, 2009 at 00:00 to October 13th, 2009 at 23:00.")+
  theme(legend.position = "bottom")



anim_save("",animation=last_animation(),path="C:/Users/vergarab/Documents/RESEARCH/3 Min Figure/Ramping_Behavior.gif")
