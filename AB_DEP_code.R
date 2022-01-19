#Apalachicola Bay analyses of DEP data


#NEERS 5007 A from https://dev.seacar.waterinstitute.usf.edu/programs/details/5007


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


d1 <- read.csv("~/GitHub/AB_NFWF1/Data_5007A_Final.csv")

names(d1)
head(d1)

#subset the columns to the ones you want to work with
d2 <- d1 %>% 
  dplyr::select(Harvested, Site, Quadrat, Weight_kg, Total_Adults_75mm, 
                Total_Seed_26_74mm, Total_Spat_0_25mm, Total_Live)

#rename headers  

names(d2)[]<-c("Date", "Site", "Quadrat", "Weight", "Legal", "Sublegal", "Spat", "Total_Live")


head(d2)