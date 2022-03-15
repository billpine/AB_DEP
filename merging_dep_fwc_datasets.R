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


d1$Bottom<-"Rock"
d1$Cultch<-200 #from jonathan and reports
d1$Project<-"NRDA_4044"

d1 <- dplyr::rename(d1,Season=season)

names(d1)
unique(d1$Bay)


# #some renaming so two DEP datasets match name columns
# d1.1 <- dplyr::rename(d1,Date=Harvested, Weight=Weight_kg, Legal=Adults_75mm, Sublegal= Seed_26_74mm, Spat=Spat_0_25mm)

#subset the columns to the ones you want to work with
d1.2 <- d1 %>% 
  dplyr::select(Site, Quadrat, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

min(d1.2$Year)


#now 5077

d2 <- read.csv("~/GitHub/AB_DEP/5007_to_merge.csv")

d2$Bottom<-"Rock"
d2$Cultch<-300
d2$Project<-"NRDA_5007"

d2 <- dplyr::rename(d2,Season=season)

min(d2$Year)

d2.1 <- d2 %>% 
  dplyr::select(Site, Quadrat, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

names(d2.1)


#merge 4044 and 5007

d3<-rbind(d1.2, d2.1)



#jonathan with dep provided a "master" spreadsheet for the dep nrda project
#this duplicates the data from the dep server I've already cleaned
#EXCEPT for the last year (2021,sheet 7) of his files so
#now I'm going to bring in the master dep file but
#only work with sheet 7

#now need to merge d4 which is project 4044 year = 2021
# to the other years of 4044 and project 5077


d4 <- read.csv("~/GitHub/AB_DEP/4044_yr2021_to_merge.csv")

d4$Bottom<-"Rock"
d4$Cultch<-200
d4$Project<-"NRDA_4044"


names(d3)
names(d4)

#more cleaning is needed


#ok reduce number of columns in d4
#subset the columns to the ones you want to work with
d4.1 <- d4 %>% 
  dplyr::select("Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Month","Day","Year","Period",
                "Season","Bottom","Cultch","Project")
str(d3)
str(d4.1)

#work on some more column names
names(d4.1)


names(d4.1)[3] <- "Weight"
names(d4.1)[4] <- "Legal"
names(d4.1)[5] <- "Sublegal"
names(d4.1)[6] <- "Spat"

names(d4.1)
names(d3)

#first lets send 4044 year 2021 to DEP folder to 
#merge with the 3 bays file, this will be all 
#3 bays all years for 4044

write.table(d4.1, file = "~/GitHub/AB_DEP/DEP_Apalach_4044_yr2021.csv", row.names = FALSE,col.names = TRUE,sep = ",")


#merge the dep files together for the two main studies 4044 and 5007

d5<-rbind(d3, d4.1)
unique(d5$Year)


##bring in FWC

d6<-read.csv("~/GitHub/AB_DEP/FWC_to_merge.csv")
names(d6)

#d6 <- dplyr::rename(d6,Season=season)

#remember TotalSpat in FWC file has been converted based on Matt Davis's
#recommendations by multipling the total number of oysters counted
#by the proportion of oysters < 26 mm

d6$Bottom<-"Shell"
d6$Project<-"NFWF_1"


names(d6)

#some renaming 
#Spat is TotalSpat from FWC-NFWF
d6.1 <- dplyr::rename(d6,Site=StationName, Weight=TotalVol,Cultch=Cultch,Spat=TotalSpat, Sublegal=TotalSeed, Legal=TotalLegal)

#subset the columns to the ones you want to work with
d6.2 <- d6.1 %>% 
  dplyr::select(Site, Quadrat, Weight, Spat, Sublegal, Legal, Year, Month, Day, Period, Season, Bottom, Cultch, Project)

#d6.2$Legal<-NA
#d6.2$Sublegal<-NA

str(d6.2)
str(d5)

#merge fwc and dep

d7<-rbind(d6.2, d5)

#Put columns in order
d7<-d7[,c("Project","Year", "Month", "Day", "Site", "Period", "Season", "Bottom", "Cultch", "Quadrat", "Weight", "Spat", "Sublegal", "Legal")]

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

e1<-read.csv("~/GitHub/AB_DEP/FWC_2021_to_merge.csv")
names(e1)


#remember TotalSpat in FWC file has been converted based on Matt Davis's
#recommendations by multiplying the total number of oysters counted
#by the proportion of oysters < 26 mm

e1$Bottom<-"Rock"
e1$Project<-"FWC_2021"
e1$Cultch<-0

names(e1)

#some renaming 
#Spat is TotalSpat from FWC-NFWF
e1.1 <- dplyr::rename(e1,Weight=Weight_kg,Spat=TotalSpat, Sublegal=TotalSeed, Legal=TotalLegal)

#subset the columns to the ones you want to work with
e1.2 <- e1.1 %>% 
  dplyr::select(StationName, Quadrat, Weight, Spat, Sublegal, Legal, Year, Month, Day, Period, Season, Bottom, Project,Cultch)

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
e1.8<-e1.8[,c("Project","Year", "Month", "Day", "Site", "Period", "Season", "Bottom","Cultch", "Quadrat", "Weight", "Spat", "Sublegal", "Legal")]



#d7.28 is FWC and DEP (not FWC 2021 data, that is in e)

str(d7.26)
str(e1.8)


f1<-rbind(d7.26, e1.8)

#this is the all of the DEP data (including 2021 from DEP directly)
#and FWC 2021 from Estes


#merged FWC and DEP
write.table(f1, file = "~/GitHub/AB_DEP/20220305_merged_agency_data.csv", row.names = FALSE,col.names = TRUE,sep = ",")
