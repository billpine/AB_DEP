#this is the 2021 Apalach file from Jim Estes. This appears to include all sites

library(readxl)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(MASS)
library(reshape)
library(ggplot2)
library(lubridate)
library(cowplot)


##bring in FWC 2021

e1<-read.csv("~/Git/AB_DEP/FWC_2021_to_merge.csv")
names(e1)


#remember TotalSpat in FWC file has been converted based on Matt Davis's
#recommendations by multipling the total number of oysters counted
#by the proportion of oysters < 26 mm

e1$Bottom<-"Rock"
e1$Project<-"FWC_2021"


names(e1)

#some renaming 
#Spat is TotalSpat from FWC-NFWF
e1.1 <- dplyr::rename(e1,Weight=Weight_kg,Spat=TotalSpat, Sublegal=TotalSeed, Legal=TotalLegal)

#subset the columns to the ones you want to work with
e1.2 <- e1.1 %>% 
  dplyr::select(Site, StationName, Quadrat, Weight, Spat, Sublegal, Legal, Year, Month, Day, Period, Season, Bottom, Project)

unique(e1.2$StationName)

#Start here fixing names
#you are working to move this back into the "merging_dep_fwc" file by
#reading this file in

e1.3<-e1.2 %>%
  mutate(Site = replace(StationName,StationName == "Easthole #7", "Easthole 7"))
e1.4<-e1.3 %>%
  mutate(Site = replace(StationName,StationName == "Lighthouse", "Lighthouse Bar"))
e1.5<-e1.4 %>%
  mutate(Site = replace(StationName,StationName == "Hotel", "Hotel Bar 1"))


#write 2021 file
#write.table((unique(e1.5$Site)), file = "~/Git/AB_DEP/name_check.csv", row.names = FALSE,col.names = TRUE,sep = ",")

