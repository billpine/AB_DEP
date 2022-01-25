#Apalachicola oyster data from two DEP files and 1 FWC file 
#data from quadrats
#Bill Pine
#January 2022


library(readxl)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(MASS)
library(sjPlot)
library(reshape)
library(ggplot2)
library(lubridate)
library(AICcmodavg)
library(ggeffects)
library(cowplot)


d1 <- read.csv("~/GitHub/AB_DEP/20220125_merged_agency_data.csv")

names(d1)


#plot
f1<-ggplot(d1, aes(x=Period, y= Spat, color=Bottom)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Live Spat") 
#+
#  ylim(0,30000)+
  xlab("Period") +
  ylab("Live oyster spat") 
#+
#  facet_wrap(~Period)
f1

