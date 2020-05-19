library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RColorBrewer)
#' Get Tableau's 10-color palette
{#' 
#' \code{colors_tableau10} returns a vector of RGB hex triplets representing
#' Tableau's 10-color color palette.
#' 
#' @export
#' @rdname tableau10
#' @note These values come from: \url{http://tableaufriction.blogspot.ro/2012/11/finally-you-can-use-tableau-data-colors.html}
colors_tableau10 <- function()
{
  return(c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B",
           "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"))
}


#' \code{colors_tableau10_light} returns a vector of RGB hex triplets
#' representing Tableau's lightly saturated 10-color color palette.
#' 
#' @export
#' @rdname tableau10
colors_tableau10_light <- function()
{
  return(c("#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5", "#C49C94",
           "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5"))
}


#' \code{colors_tableau10_light} returns a vector of RGB hex triplets
#' representing Tableau's medium saturated 10-color color palette.
#' 
#' @export
#' @rdname tableau10
colors_tableau10_medium <- function()
{
  return(c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D", "#AD8BC9", "#A8786E",
           "#ED97CA", "#A2A2A2", "#CDCC5D", "#6DCCDA"))
}
}

#ajl_line() Function
{ajl_line<-function(caption_align=1){
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=caption_align),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black",margin = margin(t = 10, b = 5)),
  )
}}


MO<-read.csv("C:/Users/vergarab/Documents/RESEARCH/My papers/MeritOrder_BUENO.csv",header = TRUE,
             na.strings = 999.99,stringsAsFactors = F)


ggplot(subset(MO,he==10),aes(merit,price,fill=offer_sum)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta Power Plant Offers and Power Prices\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+  
  scale_color_manual("",values=c("black","firebrick","blue"))+  
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12300))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(MO$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data, graph by Andrew Leach.")

ggplot(subset(MO,he==10),aes(merit,price,fill=Plant_Type)) +
  geom_rect(mapping=aes(xmin=merit-size,xmax=merit,ymin=-10,ymax=price))+
  ggtitle("Alberta's Merit Order\n")+
  geom_hline(aes(yintercept=actual_posted_pool_price,colour="Pool Price"),linetype=2,size=1)+
  scale_fill_manual("",values=colors_tableau10())+  
  scale_color_manual("",values=c("black","firebrick","blue"))+  
  scale_x_continuous(expand=c(0,0),breaks = seq(0,12000,2000),limits = c(0,12300))+
  scale_y_continuous(expand=c(0,0),breaks=seq(0,1000,100),limits=c(-20,max(MO$price)))+
  guides(fill=guide_legend(nrow=2))+
  ajl_line()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  labs(x=paste("Offered Generation (MW)"),y="Price ($/MWh)",
       #title=paste("Alberta Energy Coal Plant Merit Order by Offer Control, ",max(df1$date)," ",max(as.character(df1$he)),":00",sep = ""),
       caption="Source: AESO Data for August 10th, 2018, graph by Andrew Leach modified by Natalia Vergara (THANKS Dr. Leach!).")
