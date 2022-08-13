##
##THIS IS the DEP project for Apalach
##THIS IS the DEP project 4044 master data file
##I received this from DEP jan 25, 2022 
##

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

library(readxl)

#start round 1

d1 <- read_excel("ApalachicolaNRDAMonitoringData_MASTER.xlsx", 
                 sheet = "Round 1 Oyster Counts", col_types = 
                   c("text","date", "date", "text", "text", "text", 
                     "text", "text", "text", "text", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "text"))

names(d1)
head(d1)
names(d1)
head(d1)

d1$Date<-as.POSIXct(d1$Harvested, format="%Y-%m-%d")

d1.1 <- d1 %>%
  mutate(Year = year(d1$Date),
         Month = month(d1$Date),
         Day = day(d1$Date))

str(d1.1)

d1<-d1.1


names(d1)
str(d1)

as.numeric(d1$Year)
as.numeric(d1$Month)
as.numeric(d1$Day)

names(d1)[12] <- "Weight_kg"
names(d1)[13] <- "Adults"
names(d1)[14] <- "Seed"
names(d1)[15] <- "Spat"

#subset the columns to the ones you want to work with
d1.1 <- d1 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

names(d1.1)

str(d1.1)

unique(d1.1$Year)

##############
#start round 2

d2 <- read_excel("ApalachicolaNRDAMonitoringData_MASTER.xlsx", 
                 sheet = "Round 2 Oyster Counts", col_types = 
                   c("text","date", "date", "text", "text", "text", 
                     "text", "text", "text", "text", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "text"))

names(d2)
head(d2)
names(d2)
head(d2)

d2$Date<-as.POSIXct(d2$Harvested, format="%Y-%m-%d")

d2.1 <- d2 %>%
  mutate(Year = year(d2$Date),
         Month = month(d2$Date),
         Day = day(d2$Date))

str(d2.1)

d2<-d2.1

names(d2)
str(d2)

as.numeric(d2$Year)
as.numeric(d2$Month)
as.numeric(d2$Day)

names(d2)[12] <- "Weight_kg"
names(d2)[13] <- "Adults"
names(d2)[14] <- "Seed"
names(d2)[15] <- "Spat"

#subset the columns to the ones you want to work with
d2.1 <- d2 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

names(d2.1)

str(d2.1)

unique(d2.1$Year)


##############
#start round 3

d3 <- read_excel("ApalachicolaNRDAMonitoringData_MASTER.xlsx", 
                 sheet = "Round 3 Oyster Counts", col_types = 
                   c("text","date", "date", "text", "text", "text", 
                     "text", "text", "text", "text", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "text"))

names(d3)
head(d3)
names(d3)
head(d3)

d3$Date<-as.POSIXct(d3$Harvested, format="%Y-%m-%d")

d3.1 <- d3 %>%
  mutate(Year = year(d3$Date),
         Month = month(d3$Date),
         Day = day(d3$Date))

str(d3.1)

d3<-d3.1

names(d3)
str(d3)

as.numeric(d3$Year)
as.numeric(d3$Month)
as.numeric(d3$Day)

names(d3)[12] <- "Weight_kg"
names(d3)[13] <- "Adults"
names(d3)[14] <- "Seed"
names(d3)[15] <- "Spat"

#subset the columns to the ones you want to work with
d3.1 <- d3 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

names(d3.1)

str(d3.1)
#######
##############
#start round 4

d4 <- read_excel("ApalachicolaNRDAMonitoringData_MASTER.xlsx", 
                 sheet = "Round 4 Oyster Counts", col_types = 
                   c("text","date", "date", "text", "text", "text", 
                     "text", "text", "text", "text", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "text"))

names(d4)
head(d4)
names(d4)
head(d4)

d4$Date<-as.POSIXct(d4$Harvested, format="%Y-%m-%d")

d4.1 <- d4 %>%
  mutate(Year = year(d4$Date),
         Month = month(d4$Date),
         Day = day(d4$Date))

str(d4.1)

d4<-d4.1

names(d4)
str(d4)

as.numeric(d4$Year)
as.numeric(d4$Month)
as.numeric(d4$Day)

names(d4)[12] <- "Weight_kg"
names(d4)[13] <- "Adults"
names(d4)[14] <- "Seed"
names(d4)[15] <- "Spat"

#subset the columns to the ones you want to work with
d4.1 <- d4 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

names(d4.1)

str(d4.1)


#################
d5<-rbind(d4.1,d3.1,d2.1,d1.1)

str(d5)


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
#So April through September is summer and October through March is winter

names(d5)

unique(d5$Year)
unique(d5$Month)



d5$Period <- NA
firstyear <- 2015 
#this is the first year of the FWC NFWF, so doing this to match the periods
endyear <- max(d5$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(d5)){
    if(d5$Year[j] == y & d5$Month[j] > 3 & d5$Month[j] < 10) d5$Period[j] = p[1] #year i months 4-9
    if(d5$Year[j] == y & d5$Month[j] > 9) d5$Period[j] = p[2] #year i months 10-12
    if(d5$Year[j] == y+1 & d5$Month[j] < 4) d5$Period[j] = p[2] #year i+1 months 1-3
  }
}

d5$Season <- "Winter"
d5$Season[d5$Period == 1 | d5$Period == 3 | d5$Period == 5 | d5$Period == 7 | d5$Period == 9| d5$Period == 11| d5$Period == 13] <- "Summer"

unique(d5$Period)
#periods 13 14 10  9  6  7  5  3 only

#ok what are our site names?
unique(d5$Site)
#not lots of sites

####

d5.1<-d5 %>%
   mutate(Site = replace(Site,Site == "Redfish Creek 1", "Redfish Creek"))
d5.2<-d5.1 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek 2", "Redfish Creek"))
d5.3<-d5.2 %>%
  mutate(Site = replace(Site,Site == "Eleven Mile North", "Eleven Mile"))
d5.4<-d5.3 %>%
  mutate(Site = replace(Site,Site == "Eleven Mile South", "Eleven Mile"))
d5.5<-d5.4 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek #1", "Redfish Creek"))
d5.6<-d5.5 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek #2", "Redfish Creek"))
d5.7<-d5.6 %>%
  mutate(Site = replace(Site,Site == "Normans Bar North", "Normans Bar"))
d5.8<-d5.7 %>%
  mutate(Site = replace(Site,Site == "Normans Bar Middle", "Normans Bar"))
d5.9<-d5.8 %>%
  mutate(Site = replace(Site,Site == "Norman's Bar Middle", "Normans Bar"))
d5.10<-d5.9 %>%
  mutate(Site = replace(Site,Site == "Norman's Bar North", "Normans Bar"))
d5.11<-d5.10 %>%
  mutate(Site = replace(Site,Site == "Lighthouse Bar", "Lighthouse"))



d6<-d5.11

#ok let's now write d6 (Apalach by DEP) to a file and then that will be the file
#we merge with the others

write.table(d6, file = "20220327_Apalachicola_NRDA_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")

