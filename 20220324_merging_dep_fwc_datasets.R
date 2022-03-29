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
#first the two DEP 4044 and 5077
#then the FWC NFWF1 and NFWF 2021

#start with Pensacola

d1 <- read.csv("~/Git/AB_DEP/20220324_Pensacola_NRDA_to_merge.csv")

d1$Bottom<-"Shell"
d1$Cultch<-200 #from jonathan and reports
d1$Project<-"NRDA_4044"

names(d1)
unique(d1$Bay)


#now St.Andrews

d2 <- read.csv("~/Git/AB_DEP/20220324_StAndrews_NRDA_to_merge.csv")

d2$Bottom<-"Shell"
d2$Cultch<-200
d2$Project<-"NRDA_4044"

#now Apalach

d2.1 <- read.csv("~/Git/AB_DEP/20220327_Apalachicola_NRDA_to_merge.csv")

d2.1$Bottom<-"Rock"
d2.1$Cultch<-200
d2.1$Project<-"NRDA_4044"


##merge Pensacola and St. Andrews and Apalch NRDA
d3<-rbind(d1,d2,d2.1)
d3 <- dplyr::rename(d3,Legal=Adults)

d3.1 <- d3 %>% 
  dplyr::select(Bay,Site, Quadrat, Weight_kg, Legal, Seed, Spat, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

#now apalch 5007
#now 5077

d4 <- read.csv("~/Git/AB_DEP/5007_to_merge.csv")

d4$Bay<-"Apalachicola"
d4$Bottom<-"Rock"
d4$Cultch<-300
d4$Project<-"NRDA_5007"

d4 <- dplyr::rename(d4,Seed=Sublegal)
d4 <- dplyr::rename(d4,Weight_kg=Weight)


min(d4$Year)

d4.1 <- d4 %>% 
  dplyr::select(Bay,Site, Quadrat, Weight_kg, Legal, Seed, Spat, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

names(d4.1)

#merge 4044 and 5007

str(d3.1)
str(d4.1)

d5<-rbind(d3.1, d4.1)

#in the previous data cleaning effort Jonathan with DEP provided
#a separate file for the 2021 data from Apalach, those data
#are in the "new" master files provided by Jonathan that corrected
#errors identified after my original cleaning, so I don't have to
#bring in a separate 2021 file.

unique(d5$Site)

##bring in FWC

d6<-read.csv("~/Git/AB_DEP/FWC_to_merge.csv")
names(d6)

#d6 <- dplyr::rename(d6,Season=season)

#remember TotalSpat in FWC file has been converted based on Matt Davis's
#recommendations by multipling the total number of oysters counted
#by the proportion of oysters < 26 mm

d6$Bottom<-"Shell"
d6$Project<-"NFWF_1"
d6$Bay<-"Apalachicola"


names(d6)

#some renaming 
#Spat is TotalSpat from FWC-NFWF
d6.1 <- dplyr::rename(d6,Site=StationName, Weight_kg=TotalVol,Cultch=Cultch,Spat=TotalSpat, Seed=TotalSeed, Legal=TotalLegal)

#subset the columns to the ones you want to work with
d6.2 <- d6.1 %>% 
  dplyr::select(Bay,Site, Quadrat, Weight_kg, Spat, Seed, Legal, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

#d6.2$Legal<-NA
#d6.2$Sublegal<-NA

str(d6.2)
str(d5)

#merge fwc and dep

names(d5)
names(d6.2)

d7<-rbind(d6.2, d5)

#Put columns in order
d7<-d7[,c("Bay","Project","Year", "Month", "Day", "Site", "Period", "Season", "Bottom", "Cultch", "Quadrat", "Weight_kg", "Spat", "Seed", "Legal")]

#ok what are our site names?
unique(d7$Site)
#lots of sites

####
#Fix some name errors or inconsistencies like removing extra space



d7.1<-d7 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek #1", "Redfish Creek"))
d7.2<-d7.1 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek #2", "Redfish Creek"))
d7.3<-d7.2 %>%
  mutate(Site = replace(Site,Site == "Norman's Bar Middle", "Normans Bar Middle"))
d7.4<-d7.3 %>%
  mutate(Site = replace(Site,Site == "Norman's Bar North", "Normans Bar North"))
d7.5<-d7.4 %>%
  mutate(Site = replace(Site,Site == "East Hole #2", "East Hole 2"))
d7.6<-d7.5 %>%
  mutate(Site = replace(Site,Site == "East Hole #1", "East Hole 1"))
d7.7<-d7.6 %>%
  mutate(Site = replace(Site,Site == "NFWF Hotel Bar", "Hotel Bar"))
d7.8<-d7.7 %>%
  mutate(Site = replace(Site,Site == "NFWF Dry Bar", "Dry Bar"))
d7.9<-d7.8 %>%
  mutate(Site = replace(Site,Site == "NFWF Bulkhead", "Bulkhead"))
d7.10<-d7.9 %>%
  mutate(Site = replace(Site,Site == "Monkey's Elbow", "Monkeys Elbow"))
d7.11<-d7.10 %>%
  mutate(Site = replace(Site,Site == "Cabbage Lumps ", "Cabbage Lumps"))
d7.12<-d7.11 %>%
  mutate(Site = replace(Site,Site == "Normans Bar North", "Normans"))
d7.13<-d7.12 %>%
  mutate(Site = replace(Site,Site == "Normans Bar Middle", "Normans"))
d7.14<-d7.13 %>%
  mutate(Site = replace(Site,Site == "Hotel Bar 2", "Hotel"))
d7.15<-d7.14 %>%
  mutate(Site = replace(Site,Site == "Hotel Bar 1", "Hotel"))
d7.16<-d7.15 %>%
  mutate(Site = replace(Site,Site == "East Hole 1", "East Hole"))
d7.17<-d7.16 %>%
  mutate(Site = replace(Site,Site == "East Hole 2", "East Hole"))
d7.18<-d7.17 %>%
  mutate(Site = replace(Site,Site == "Easthole", "East Hole"))
d7.19<-d7.18 %>%
  mutate(Site = replace(Site,Site == "Eleven Mile North", "Eleven Mile"))
d7.20<-d7.19 %>%
  mutate(Site = replace(Site,Site == "Eleven Mile South", "Eleven Mile"))
d7.21<-d7.20 %>%
  mutate(Site = replace(Site,Site == "Hotel", "Hotel Bar"))
d7.22<-d7.21 %>%
  mutate(Site = replace(Site,Site == "Lighthouse", "Lighthouse Bar"))
d7.23<-d7.22 %>%
  mutate(Site = replace(Site,Site == "North Spur 2", "North Spur"))
d7.24<-d7.23 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek 1", "Redfish Creek"))
d7.25<-d7.24 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek 2", "Redfish Creek"))
d7.26<-d7.25 %>%
  mutate(Site = replace(Site,Site == "South Bulkhead", "Bulkhead"))


unique(d7.26$Site)

#name check file for FWC and DEP (2021 not brought in until below)
#write.table((unique(d7.11$Site)), file = "~/Git/AB_DEP/name_check.csv", row.names = FALSE,col.names = TRUE,sep = ",")

#################################################
##2021 FWC
#################################################

##bring in FWC 2021

e1<-read.csv("~/Git/AB_DEP/FWC_2021_to_merge.csv")
names(e1)


#remember TotalSpat in FWC file has been converted based on Matt Davis's
#recommendations by multiplying the total number of oysters counted
#by the proportion of oysters < 26 mm

e1$Bottom<-"Rock"
e1$Project<-"FWC_2021"
e1$Cultch<-999
e1$Bay<-"Apalachicola"

names(e1)

#some renaming 
#Spat is TotalSpat from FWC-NFWF
e1.1 <- dplyr::rename(e1,Weight_kg=Weight_kg,Spat=TotalSpat, Seed=TotalSeed, Legal=TotalLegal)

#subset the columns to the ones you want to work with
e1.2 <- e1.1 %>% 
  dplyr::select(Bay,StationName, Quadrat, Weight_kg, Spat, Seed, Legal, Year, Month, Day, Period, Season, Bottom, Project,Cultch)

unique(e1.2$StationName)

#Start here fixing names
#you are working to move this back into the "merging_dep_fwc" file by
#reading this file in

e1.3<-e1.2 %>%
  mutate(StationName = replace(StationName,StationName == "Easthole #7", "East Hole"))
e1.4<-e1.3 %>%
  mutate(StationName = replace(StationName,StationName == "Lighthouse Bar", "Lighthouse Bar"))
e1.5<-e1.4 %>%
  mutate(StationName = replace(StationName,StationName == "Hotel Bar 1", "Hotel Bar"))
e1.6<-e1.5 %>%
  mutate(StationName = replace(StationName,StationName == "Dry Bar North", "Dry Bar"))
e1.7<-e1.6 %>%
  mutate(StationName = replace(StationName,StationName == "Hotel", "Hotel Bar"))

e1.8 <- dplyr::rename(e1.7,Site=StationName)

unique(e1.8$Site)

#Put columns in order
e1.8<-e1.8[,c("Bay","Project","Year", "Month", "Day", "Site", "Period", "Season", "Bottom","Cultch", "Quadrat", "Weight_kg", "Spat", "Seed", "Legal")]

#d7.28 is FWC and DEP (not FWC 2021 data, that is in e)

str(d7.26)
str(e1.8)


f1<-rbind(d7.26, e1.8)

#this is the all of the DEP data (including 2021 from DEP directly)
#and FWC 2021 from Estes
#this includes working with the DEP data that were in error in SECAR and 
#instead I use the updated files from Jonathan directly


#merged FWC and DEP
write.table(f1, file = "~/Git/AB_DEP/20220326_merged_agency_data.csv", row.names = FALSE,col.names = TRUE,sep = ",")
