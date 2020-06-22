#sgdthbgjngnjuk
#install.packages("tidyverse")
#install.packages("tidyr")
install.packages("dplyr")
install.packages("magrittr")

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


setwd("Z:/StudyI/Nick_Simulation/Simulation_mod/Simulation_new2_notvc_a/8.Bias_tables/Boxplots_R")
##########################################################################################
#Boxplot
#############################################################################################
data <- read.csv("R_long.csv", header = TRUE, sep = ",", dec = ".")
data[1:10,]
attach(data)

data$t5  <- ifelse(year ==5, TRUE, FALSE)
data$t10 <- ifelse(year ==10, TRUE, FALSE)



#####A Average of each approach over all the scenarios for females over 70 and 80###
png("boxplot_CIF1_overall.png", width=1000)

ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data) +
  geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                       xlab("Scenario")+ylab("Bias_CIF1")+
                                       facet_wrap(~as.factor(givenage), nrow=5)+ ylim(-0.025, 0.025)
dev.off()

png("boxplot_CIF2_overall.png", width=1000)
ggplot(aes(y = bias_CIF2_at, x = factor(scenario), fill = as.factor(model)), data = data) +
                                       geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                       xlab("Scenario")+ylab("Bias_CIF2")+
                                       facet_wrap(~as.factor(givenage), nrow=5)+ ylim(-0.05, 0.05)
dev.off()
##########################################################################################################



#####B) Factors influence 

png("boxplot_CIF2_independence.png", width=1000)
ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data) +
  geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                        xlab("Scenario")+ylab("Bias_CIF2")+
                                        facet_grid(cols = vars(givenage), rows=vars(independence))+ ylim(-0.02, 0.02)
dev.off()


png("boxplot_CIF2_PH.png", width=1000)
ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data) +
  geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                       xlab("Scenario")+ylab("Bias_CIF2")+
                                       facet_grid(cols = vars(givenage), rows=vars(prop_sex_other))+ ylim(-0.02, 0.02)
dev.off()



png("boxplot_CIF2_sdage.png", width=1000)
ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data) +
  geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                       xlab("Scenario")+ylab("Bias_CIF2")+
                                       facet_grid(cols = vars(givenage), rows=vars(sdage))+ ylim(-0.015, 0.015)
dev.off()



png("boxplot_CIF2_indep.png", width=1000)
ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data) +
  geom_boxplot()+  scale_fill_discrete(name="model",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
                                       xlab("Scenario")+ylab("Bias_CIF2")+
                                       facet_grid(cols = vars(givenage), rows=vars(independence))+ ylim(-0.015, 0.015)

dev.off()

#### Tree diagram
#install.packages("DiagrammeR")
library(data.tree)

tree <- Node$new("Scenarios")
sdage12 <- tree$AddChild("sdage=12")
sdage20 <- tree$AddChild("sdage=20")

Weibull_12 <- sdage12$AddChild("Weibull")
Weibull_20 <- sdage20$AddChild("Weibull")

Nonexp_12 <- sdage12$AddChild("Non exponential")
Nonexp_20 <- sdage20$AddChild("Non exponential")

Gompertz_12 <- sdage12$AddChild("Gompertz")
Gompertz_20 <- sdage20$AddChild("Gompertz")

  dep_Weibull_12   <- Weibull_12$AddChild("Dependence")
indep_Weibull_12   <- Weibull_12$AddChild("Independence")
  dep_Weibull_20   <- Weibull_20$AddChild("Dependence")
indep_Weibull_20   <- Weibull_20$AddChild("Independence")


dep_Nonexp_12      <- Nonexp_12$AddChild("Dependence")
indep_Nonexp_12    <- Nonexp_12$AddChild("Independence")
dep_Nonexp_20      <- Nonexp_20$AddChild("Dependence")
indep_Nonexp_20    <- Nonexp_20$AddChild("Independence")


dep_Gompertz_12    <-Gompertz_12$AddChild("Dependence")
indep_Gompertz_12  <- Gompertz_12$AddChild("Independence")
dep_Gompertz_20    <- Gompertz_20$AddChild("Dependence")
indep_Gompertz_20  <-Gompertz_20$AddChild("Independence")



PH_dep_Weibull_12     <-dep_Weibull_12$AddChild("PH gender")
NonPH_dep_Weibull_12  <-dep_Weibull_12$AddChild("Non PH gender")

PH_indep_Weibull_12   <-indep_Weibull_12$AddChild("PH gender")
NonPH_indep_Weibull_12<-indep_Weibull_12$AddChild("Non PH gender")

PH_dep_Weibull_20     <-dep_Weibull_20$AddChild("PH gender")
NonPH_dep_Weibull_20  <-dep_Weibull_20$AddChild("Non PH gender")

PH_indep_Weibull_20   <-indep_Weibull_20$AddChild("PH gender")
NonPH_indep_Weibull_20<-indep_Weibull_20$AddChild("Non PH gender")

PH_dep_Nonexp_12      <-dep_Nonexp_12$AddChild("PH gender") 
NonPH_dep_Nonexp_12   <-dep_Nonexp_12$AddChild("Non PH gender") 

PH_indep_Nonexp_12    <-indep_Nonexp_12$AddChild("PH gender") 
NonPH_indep_Nonexp_12 <-indep_Nonexp_12$AddChild("Non PH gender") 

PH_dep_Nonexp_20      <-dep_Nonexp_20$AddChild("PH gender")  
NonPH_dep_Nonexp_20   <-dep_Nonexp_20$AddChild("Non PH gender")  

PH_indep_Nonexp_20    <-indep_Nonexp_20$AddChild("PH gender")  
NonPH_indep_Nonexp_20 <-indep_Nonexp_20$AddChild("Non PH gender")

PH_dep_Gompertz_12      <-dep_Gompertz_12$AddChild("PH gender")   
NonPH_dep_Gompertz_12   <-dep_Gompertz_12$AddChild("Non PH gender")  

PH_indep_Gompertz_12    <-indep_Gompertz_12$AddChild("PH gender")  
NonPH_indep_Gompertz_12 <-indep_Gompertz_12$AddChild("Non PH gender") 

PH_dep_Gompertz_20      <-dep_Gompertz_20$AddChild("PH gender")     
NonPH_dep_Gompertz_20   <-dep_Gompertz_20$AddChild("Non PH gender") 

PH_indep_Gompertz_20    <-indep_Gompertz_20$AddChild("PH gender")  
NonPH_indep_Gompertz_20 <-indep_Gompertz_20$AddChild("Non PH gender")

print(tree)

plot(sdage12)
plot(sdage20)

##########Highlight#################################################################
################################################################################################################

###############CIF1##########################################################################################
options(scipen = 999)


data$id_n =rep(1:(nrow(data)/10), times=1, each=10)

for (i in 1:(nrow(data)/10)) {data$t5_CIF1[(((i*10)-10)+1):((i*10))]= data[which(data$year==5 & data$t5==TRUE & data$id_n==i),23] }
for (i in 1:(nrow(data)/10)) {data$t10_CIF1[(((i*10)-10)+1):((i*10))]= data[which(data$year==10 & data$t10==TRUE & data$id_n==i),23] }




data70=data.frame(data[givenage==70,])


p3=ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data70) +
  geom_boxplot()+  scale_fill_discrete(name="Approaches",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
  xlab("Scenarios")+ylab("Bias CIF1") + 
  geom_rect(aes(xmin = 0,xmax =0,ymin = 0.001, ymax = 0.008, fill = "#d8161688"),size = 2 , alpha=0.001)+
  geom_rect(aes(xmin = 0,xmax =0,ymin = 0.001, ymax = 0.008, fill = "#d8161688"),size = 2 , alpha=0.001)+
  geom_rect(aes(xmin = 12,xmax =24,ymin = -0.005, ymax = -0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  labs(title = "Age at diagnosis 70")+  geom_point( position=position_dodge(width=0.75),
                                                    data=subset(data70, t5), 
                                                    aes(x= factor(scenario), y= bias_CIF1_at, fill = as.factor(model)), 
                                                    color="red", size=2,  shape=24, fill="red")  +
                                        geom_point( position=position_dodge(width=0.75),
                                                    data=subset(data70, t10), 
                                                    aes(x= factor(scenario), y= bias_CIF1_at, fill = as.factor(model)), 
                                                    color="navy", size=2, shape=22)  +
                                       stat_summary(fun.y=mean,position=position_dodge(width=0.75), colour="gray0", geom="point", 
                                                   shape=20, size=2,show_legend = TRUE, data=data70) +
  theme(panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0),
                                              axis.title.x = element_text( size=20),
                                              axis.title.y = element_text( size=20),
                                              axis.text.x = element_text( 
                                                size=15),
                                              axis.text.y = element_text( 
                                                size=15))+ 
                                              scale_y_continuous(breaks =seq( -0.02, 0.02, by = 0.005))


data80=data[givenage==80,]
p4=ggplot(aes(y = bias_CIF1_at, x = factor(scenario), fill = as.factor(model)), data = data80) +
  geom_boxplot()+  scale_fill_discrete(name="Approaches",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
  xlab("Scenarios")+ylab("Bias CIF1") + ylim(-0.02, 0.02)+
  geom_rect(aes(xmin = 0.5,xmax =4.5,ymin = 0.001, ymax = 0.008, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 7.5,xmax =12.5,ymin = 0.001, ymax = 0.008, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 13.5,xmax =14.5,ymin = 0, ymax = 0.02, fill = "#deepskyblue1"),size = 2 , alpha=0.001)+
  geom_rect(aes(xmin = 22.5,xmax =25,ymin = -0.002, ymax = 0.005, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  labs(title = "Age at diagnosis 80")+  theme(panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0),
                                              axis.title.x = element_text( size=20),
                                              axis.title.y = element_text( size=20),
                                              axis.text.x = element_text( 
                                                size=15),
                                              axis.text.y = element_text( 
                                                size=15))

png("boxplot_CIF1_overall.png",height=1000, width=1000)

grid.arrange(p3,p4, nrow=2,  heights=c(1,1),top=textGrob("Figure 1: Boxplots of bias over the 10 years of follow-up time in CIF1 from each approach over the scenarios",x = 0,y = 0.5, just = "left",gp=gpar(fontsize=16,font=1)),
             bottom = 
               textGrob("
Yellow color for age 80: All scenarios with PH for gender appear to have sligtly bigger bias than those with non PH (all PH- nonPH pairs, eg.23,24)
Red color for age 70: For Scenarios with increased a0 variance the bias of linear approach incereases
Red color for age 80: For the scenarios under Weibull and Gompertz, when the a0 variance is small, the bias of linear approach is neglectible ",
                        x = 0,y = 0.5, just = "left"))

dev.off()


p3_ly <- ggplotly(p3)
p3_ly

p4_ly <- ggplotly(p4)
p4_ly


###############CIF2##########################################################################################


for (i in 1:(nrow(data)/10)) {data$t5_CIF2[(((i*10)-10)+1):((i*10))]= data[which(data$year==5 & data$t5==TRUE & data$id_n==i),24] }
for (i in 1:(nrow(data)/10)) {data$t10_CIF2[(((i*10)-10)+1):((i*10))]= data[which(data$year==10 & data$t10==TRUE & data$id_n==i),24] }





data70=data[givenage==70,]
p1=ggplot(aes(y = bias_CIF2_at, x = factor(scenario), fill = as.factor(model)), data = data70) +
  geom_boxplot()+  scale_fill_discrete(name="Approaches",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
  xlab("Scenarios")+ylab("Bias CIF2") +
geom_rect(  aes(xmin = 12.5,xmax =13.5,ymin = 0, ymax = 0.02, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 14.5,xmax =15.5,ymin = 0, ymax = 0.02, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 20.5,xmax =21.5,ymin = 0, ymax = 0.02, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 22.5,xmax =23.5,ymin = 0, ymax = 0.02, fill ="#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 13.5,xmax =14.5,ymin = 0, ymax = 0.02, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 15.5,xmax =16.5,ymin = 0, ymax = 0.02, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 21.5,xmax =22.5,ymin = 0, ymax = 0.02, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 23.5,xmax =24.5,ymin = 0, ymax = 0.02, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 5.5 ,xmax =6.5, ymin = 0, ymax = 0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 7.5 ,xmax =8.5, ymin = 0, ymax = 0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 17.5,xmax =18.5,ymin = 0, ymax = 0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 19.5,xmax =20.5,ymin = 0, ymax = 0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  ylim(-0.02, 0.15)+ labs(title = "Age at diagnosis 70")+  theme(panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0),
                                                                  axis.title.x = element_text( size=20),
                                                                  axis.title.y = element_text( size=20),
                                                                  axis.text.x = element_text( 
                                                                    size=15),
                                                                  axis.text.y = element_text( 
                                                                    size=15))






data80=data[givenage==80,]
p2=ggplot(aes(y = bias_CIF2_at, x = factor(scenario), fill = as.factor(model)),position = position_stack(), data = data80) +
  geom_boxplot()+  scale_fill_discrete(name="Approaches",
                                       breaks=c(0,1,2,3),
                                       labels=c("Attained", "Linear", "Splines","Splint"))+
  xlab("Scenarios")+ylab("Bias CIF2") +
  geom_rect(aes(xmin = 12.5,xmax =13.5,ymin = -0.03, ymax = 0, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 14.5,xmax =15.5,ymin = -0.03, ymax = 0, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 20.5,xmax =21.5,ymin = -0.03, ymax = 0, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 22.5,xmax =23.5,ymin = -0.03, ymax = 0, fill = "#d8161688"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 1.5,xmax =2.5, ymin = 0,ymax = 0.015, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 3.5,xmax =4.5,ymin = 0, ymax = 0.015, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 9.5,xmax =10.5,ymin = 0, ymax = 0.015, fill ="#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 11.5,xmax =12.5,ymin = 0, ymax = 0.015, fill = "#deepskyblue1"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 17.5,xmax =18.5,ymin = -0.003, ymax = 0.02, fill ="#springgreen2"),size = 2 , alpha=0.006)+
  geom_rect(aes(xmin = 19.5,xmax =20.5,ymin = -0.002, ymax = 0.02, fill = "#springgreen2"),size = 2 , alpha=0.006)+
  ylim(-0.1, 0.05)+ labs(title = "Age at diagnosis 80")+  theme(panel.grid.major = element_blank(),plot.margin = margin(8,0,8,0),
                                                                axis.title.x = element_text( size=20),
                                                                axis.title.y = element_text( size=20),
                                                                axis.text.x = element_text( 
                                                                  size=15),
                                                                axis.text.y = element_text( 
                                                                  size=15))
  
png("boxplot_CIF2_overall.png",height=1000, width=1000)

grid.arrange(  p1,p2, nrow=2,  heights=c(1,1),top=textGrob("Figure 2: Boxplots of bias over the 10 years of follow-up time in CIF2 from each approach over the scenarios",x = 0,y = 0.5, just = "left",gp=gpar(fontsize=16,font=1)),
             bottom = textGrob("
Green: Scenarios where the attained age approach outperforms the splines and/or Splines+ int approach 
Red color: Scenarios where the attained age approach behaves poorer in comparison with the splines and/or Splines+ int approach
Yellow color: Scenarios where the attained age approach does not have neglectible bias (neither the spline and spline plus int approaches)",                                                             
         x = 0,y = 0.5,just = "left")      )

dev.off()


p1_ly <- ggplotly(p1)
p1_ly

p2_ly <- ggplotly(p2)
p2_ly
