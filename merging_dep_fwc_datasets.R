#This is the file to merge the two DEP datasets with the
##FWC data set and do a little more cleaning on these data sets

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




#now we will start merging the data sets
#first the two DEP
#then the FWC

#start with 4044

d1 <- read.csv("~/GitHub/AB_DEP/4044_to_merge.csv")

d1$Bottom<-"Shell"
d1$Cultch<-300

names(d1)
unique(d1$Bay)


# #some renaming so two DEP datasets match name columns
# d1.1 <- dplyr::rename(d1,Date=Harvested, Weight=Weight_kg, Legal=Adults_75mm, Sublegal= Seed_26_74mm, Spat=Spat_0_25mm)

#subset the columns to the ones you want to work with
d1.2 <- d1 %>% 
  dplyr::select(Site, Quadrat, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, season, Bottom, Cultch)

min(d1.2$Year)


#now 5077

d2 <- read.csv("~/GitHub/AB_DEP/5007_to_merge.csv")

d2$Bottom<-"Rock"
d2$Cultch<-300

min(d2$Year)

d2.1 <- d2 %>% 
  dplyr::select(Site, Quadrat, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, season, Bottom, Cultch)

names(d2.1)


#merge live count total data frame with the tran_length total data frame

d3<-rbind(d1.2, d2.1)


##bring in FWC

d4<-read.csv("~/GitHub/AB_DEP/FWC_to_merge.csv")

d4$Bottom<-"Shell"


names(d4)

#some renaming so two DEP datasets match name columns
d4.1 <- dplyr::rename(d4,Site=StationName, Weight=TotalVol,Cultch=Cultch,Spat=LiveSpat)

#subset the columns to the ones you want to work with
d4.2 <- d4.1 %>% 
  dplyr::select(Site, Quadrat, Weight, Spat, Year, Month, Day, Period, season, Bottom, Cultch)

d4.2$Legal<-NA
d4.2$Sublegal<-NA

str(d4.2)
str(d3)


d5<-rbind(d4.2, d3)

d5 <- dplyr::rename(d5,Season=season)


d5<-d5[,c("Year", "Month", "Day", "Site", "Period", "Season", "Bottom", "Cultch", "Quadrat", "Weight", "Spat", "Sublegal", "Legal")]

# #name check file for FWC and DEP
# write.table((unique(d5$Site)), file = "~/GitHub/AB_DEP/name_check.csv", row.names = FALSE,col.names = TRUE,sep = ",")

#merged FWC and DEP
write.table(d5, file = "~/GitHub/AB_DEP/20220125_merged_agency_data.csv", row.names = FALSE,col.names = TRUE,sep = ",")
