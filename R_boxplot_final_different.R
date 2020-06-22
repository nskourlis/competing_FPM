#sdgsdfg
#install.packages("tidyverse")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("magrittr")

library("tidyr")
library("dplyr")
library("ggplot2")
library(gridExtra)
library(grid)
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(DT)
library(magrittr)


setwd("C:/Users/niksko/Desktop/8.Bias_tables/Boxplots_R/")
##########################################################################################
#Boxplot
#############################################################################################
data <- read.csv("R_long.csv", header = TRUE, sep = ",", dec = ".")
data[1:10,]
attach(data)

data$t5  <- ifelse(year ==5, TRUE, FALSE)
data$t10 <- ifelse(year ==10, TRUE, FALSE)

data$bias_CIF2_at[which(data$scenario==14 & data$year==5 & data$model==0 & data$givenage==70 )]
data$bias_CIF2_at[which(data$scenario==14 & data$year==5 & data$model==0 & data$givenage==80)]



options(scipen = 999)


data$id_n =rep(1:(nrow(data)/10), times=1, each=10)

for (i in 1:(nrow(data)/10)) {data$t5_CIF1[(((i*10)-10)+1):((i*10))]= data[which(data$year==5 & data$t5==TRUE & data$id_n==i),23] }
for (i in 1:(nrow(data)/10)) {data$t10_CIF1[(((i*10)-10)+1):((i*10))]= data[which(data$year==10 & data$t10==TRUE & data$id_n==i),23] }

for (i in 1:(nrow(data)/10)) {data$t5_CIF2[(((i*10)-10)+1):((i*10))]= data[which(data$year==5 & data$t5==TRUE & data$id_n==i),24] }
for (i in 1:(nrow(data)/10)) {data$t10_CIF2[(((i*10)-10)+1):((i*10))]= data[which(data$year==10 & data$t10==TRUE & data$id_n==i),24] }


data$code = as.factor(c(rep("Age at diagnosis 70", nrow(data))))
data$model=as.factor(data$model)

data70=data.frame(data[givenage==70 & scenario<=12,])
data70$bias_CIF2_at[which(data70$scenario==1 & data70$year==5 & data70$model==0 )]

data80A=data[givenage==80 & scenario<=12,]
data80A$bias_CIF2_at[which(data80$scenario==1 & data80$year==5 & data80$model==0 )]

data80B=data[givenage==80 & scenario>12,]
data80B$bias_CIF2_at[which(data80$scenario==1 & data80$year==5 & data80$model==0 )]




###############CIF2##########################################################################################

pA=ggplot(aes(y = bias_CIF2_at, x = factor(scenario), fill = as.factor(model)), data = data80A) +
  geom_boxplot() +
  scale_fill_discrete(name="Approaches",breaks=c(0,1,2,3),
                      labels=c("Approach a-Attained","Approach b-Linear", "Approach c-Splines","Approach d-Splines/int"))+
  xlab("Scenarios")+ylab("Bias CIF2") +
  geom_rect(aes(xmin = 0.5,xmax =4.5,ymin = 0.01, ymax = 0.02, fill = "#d8161688"),size = 3 , alpha=0.006)+
  geom_rect(aes(xmin = 4.5,xmax =8.5,ymin = -0.02, ymax = -0.01, fill = "#d8161688"),size = 3 , alpha=0.006)+
  geom_rect(aes(xmin = 2,xmax =2.5,ymin = -0.01, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  geom_rect(aes(xmin = 4,xmax =4.5,ymin = -0.01, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  geom_rect(aes(xmin = 6,xmax =6.5,ymin = -0.01, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  geom_rect(aes(xmin = 8,xmax =8.5,ymin = -0.01, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  geom_rect(aes(xmin = 10,xmax =10.5,ymin = -0.015, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  geom_rect(aes(xmin = 12,xmax =12.5,ymin = -0.015, ymax = -0.005, fill = "#springgreen2"),size = 3 , alpha=0.005)+
  labs(title = "Age at diagnosis 80", 
       x = "Scenarios",
       color = "") +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x =factor(scenario), y = mean_bias_CIF1_at,  fill = model, pch = 16, color = "black"),  
             size = 2) +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y = t5_CIF2, fill = model,  pch = 16, color = "orange4"),  
             size = 2) +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y =t10_CIF2, fill = model, pch = 16, color = "red"),  
             size = 2) +
  scale_color_manual(values = c("black", "orange4", "red"),
                     labels = c("", "",""  )) +
  scale_shape_identity()+
  scale_y_continuous(breaks =seq( -0.05, 0.05, by = 0.01)) +guides(colour=FALSE) +
  theme(title= element_text( size=25),
        legend.position="top",legend.box = "vertical",legend.text = element_text( size=19),legend.title = element_text(size = 19),
        panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0), 
        axis.title.x = element_text( size=22),axis.title.y = element_text( size=22),
        axis.text.x = element_text( size=20),axis.text.y = element_text( size=18),
        legend.background = element_rect(fill="burlywood1", size=0.5, linetype="solid"))


pA



pB=ggplot(aes(y = bias_CIF2_at, x = factor(scenario), fill = factor(model)), data = data80B) +
  geom_boxplot() +
  scale_fill_discrete(name="",breaks=c(0,1,2,3),
                       labels=c("", "", "",""))+                       
  xlab("Scenarios")+ylab("Bias CIF2") + 
  geom_rect(aes(xmin = 0.5,xmax =4.5,ymin = 0.02, ymax = 0.04, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 4.5,xmax =8.5,ymin = -0.06, ymax = -0.025, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 8.5,xmax =12.5,ymin = 0.015, ymax = 0.025, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 6,xmax =6.5,ymin = 0.005, ymax = 0.015, fill = "#springgreen2"),size = 2 , alpha=0.005)+
  geom_rect(aes(xmin = 8,xmax =8.5,ymin = 0.005, ymax = 0.015, fill = "#springgreen2"),size = 2 , alpha=0.005)+
 labs(
       x = "Scenarios",
       title = "") +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x =factor(scenario), y = mean_bias_CIF2_at,  fill = model, pch = 16, color = "black"),  
             size = 2) +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y = t5_CIF2, fill = model,  pch = 16, color = "orange4"),  
             size = 2) +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y =t10_CIF2, fill = model, pch = 16, color = "red"),  
             size = 2) +
  scale_color_manual(name="",values = c("black", "orange4", "red"),
                     labels = c("Mean bias CIF2", "Bias CIF2 at time since diag= 5","Bias CIF2 at time since diag=10"  )) +
  scale_shape_identity()+
  scale_y_continuous(breaks =seq( -0.05, 0.05, by = 0.01)) + guides(fill=FALSE) +
  theme(legend.position="bottom",legend.box = "vertical",legend.text = element_text( size=19),legend.title = element_text(size = 19),
        panel.grid.major = element_blank(),
        plot.margin = margin(8,0,8,0), 
        axis.title.x = element_text( size=22),
        axis.title.y = element_text( size=22), axis.text.x = element_text( size=20),axis.text.y = element_text( size=18),
        legend.background = element_rect(fill="burlywood1", size=0.5, linetype="solid"))

pB



png("boxplot_CIF2_overall.png",height=1000, width=1000)

grid.arrange(pA ,pB , nrow=2, heights=c(1,1), bottom = 
               textGrob("",
                        x = 0,y = 0.5,gp=gpar(fontsize=25), just = "left"))

dev.off()


pdf.options(reset = TRUE)
pdf("boxplot_CIF2_overall.pdf", paper = "a4r")

grid.arrange(pA ,pB , nrow=2, heights=c(1,1), bottom = 
               textGrob("       ",
                        x = 0,y = 0.5,gp=gpar(fontsize=16), just = "left"))

dev.off()




###############CIF1##########################################################################################

pC=ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data80A) +
  geom_boxplot() +
  scale_fill_discrete(name="Approaches",breaks=c(0,1,2,3),
                      labels=c("Approach a-Attained","Approach b-Linear", "Approach c-Splines","Approach d-Splines/int"))+
  xlab("Scenarios")+ylab("Bias CIF1") +
  labs(title = "Age at diagnosis 80", 
       x = "Scenarios",
       color = "") +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x =factor(scenario), y = mean_bias_CIF1_at,  fill = model, pch = 16, color = "black"),  
             size = 2) +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y = t5_CIF1, fill = model,  pch = 16, color = "orange4"),  
             size = 2) +
  geom_point(data = data80A,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y =t10_CIF1, fill = model, pch = 16, color = "red"),  
             size = 2) +
  scale_color_manual(values = c("black", "orange4", "red"),
                     labels = c("", "",""  )) +
  scale_shape_identity()+
  scale_y_continuous(breaks =seq( -0.01, 0.01, by = 0.002)) +guides(colour=FALSE) +
  theme(title= element_text( size=25),
        legend.position="top",legend.box = "vertical",legend.text = element_text( size=19),legend.title = element_text(size = 19),
        panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0), 
        axis.title.x = element_text( size=22),axis.title.y = element_text( size=22),
        axis.text.x = element_text( size=20),axis.text.y = element_text( size=18),
        legend.background = element_rect(fill="burlywood1", size=0.5, linetype="solid"))


pC



pD=ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = factor(model)), data = data80B) +
  geom_boxplot() +
  scale_fill_discrete(name="",breaks=c(0,1,2,3),
                      labels=c("", "", "",""))+                       
  xlab("Scenarios")+ylab("Bias CIF1") + 
  labs(
    x = "Scenarios",
    title = "") +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x =factor(scenario), y = mean_bias_CIF1_at,  fill = model, pch = 16, color = "black"),  
             size = 2) +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y = t5_CIF1, fill = model,  pch = 16, color = "orange4"),  
             size = 2) +
  geom_point(data = data80B,position=position_dodge(width=0.75), mapping=aes(x = factor(scenario), y =t10_CIF1, fill = model, pch = 16, color = "red"),  
             size = 2) +
  scale_color_manual(name="",values = c("black", "orange4", "red"),
                     labels = c("Mean bias CIF1", "Bias CIF1 at time since diag= 5","Bias CIF1 at time since diag=10"  )) +
  scale_shape_identity()+
  scale_y_continuous(breaks =seq(  -0.01, 0.01, by = 0.002)) + guides(fill=FALSE) +
  theme(legend.position="bottom",legend.box = "vertical",legend.text = element_text( size=19),legend.title = element_text(size = 19),
        panel.grid.major = element_blank(),
        plot.margin = margin(8,0,8,0), 
        axis.title.x = element_text( size=22),
        axis.title.y = element_text( size=22), axis.text.x = element_text( size=20),axis.text.y = element_text( size=18),
        legend.background = element_rect(fill="burlywood1", size=0.5, linetype="solid"))

pD



png("boxplot_CIF1_overall.png",height=1000, width=1000)

grid.arrange(pC ,pD , nrow=2, heights=c(1,1), bottom = 
               textGrob("",
                        x = 0,y = 0.5,gp=gpar(fontsize=25), just = "left"))

dev.off()


pdf.options(reset = TRUE)
pdf("boxplot_CIF1_overall.pdf", paper = "a4r")

grid.arrange(pC ,pD , nrow=2, heights=c(1,1), bottom = 
               textGrob("       ",
                        x = 0,y = 0.5,gp=gpar(fontsize=16), just = "left"))

dev.off()


