#Apalachicola Bay analyses of DEP data


#The database and meta data are here 
#https://dev.seacar.waterinstitute.usf.edu/programs/details/5007

#To do integrate 4044A and 5007A 
#Project 4044 is the NRDA project and it used limestone cultch
#Project 5007 is the RESTORE project and it used shell cultch
#Plot data for each size class over time (adult, seed, spat)
#Plot biomass overtime (usually listed as total weight)
#Will need to extract dates and then create periods
#Because not all years are the same will likely need to use covariates to try and #explain difference in year such as river discharge


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