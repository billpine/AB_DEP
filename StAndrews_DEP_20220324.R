##
##THIS IS the DEP project for StAndrews
##I received this from DEP Mar 24, 2022 
#this file replaces the earlier East Bay data from SECAR which
#had errors
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

d1 <- read_excel("StAndrewsNRDAMonitoringData_MASTER - Update.xlsx", 
                                             
              sheet = "Round 1 Oyster Counts",na = ".", col_types = c("text","date", "date", "text", "text", "text","text", "text", "numeric", "numeric", 
                                                            "numeric", "text", "text", "text", "text", "text"))

str(d1)
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

names(d1)[10] <- "Weight_kg"
names(d1)[11] <- "Adults"
names(d1)[12] <- "Seed"
names(d1)[13] <- "Spat"

#subset the columns to the ones you want to work with
d1.1 <- d1 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

names(d1.1)

str(d1.1)

##############
#start round 2

d2 <- read_excel("StAndrewsNRDAMonitoringData_MASTER - Update.xlsx", 
                 
                 sheet = "Round 2 Oyster Counts",na = ".", col_types = c("text","date", "date", "text", "text", "text","text", "text", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric","numeric", "numeric", "text"))

str(d2)
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

names(d2)

names(d2)[10] <- "Weight_kg"
names(d2)[11] <- "Adults"
names(d2)[12] <- "Seed"
names(d2)[13] <- "Spat"

#subset the columns to the ones you want to work with
d2.1 <- d2 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

as.numeric(d2.1$Year)
as.numeric(d2.1$Month)
as.numeric(d2.1$Day)


##############
#start round 3

d3 <- read_excel("StAndrewsNRDAMonitoringData_MASTER - Update.xlsx", 
                 
                 sheet = "Round 3 Oyster Counts",na = ".", col_types = c("text","date", "date", "text", "text", "text","text", "text", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric","numeric", "numeric", "text"))

str(d3)
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


names(d3)

names(d3)[10] <- "Weight_kg"
names(d3)[11] <- "Adults"
names(d3)[12] <- "Seed"
names(d3)[13] <- "Spat"

#subset the columns to the ones you want to work with
d3.1 <- d3 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")

str(d3.1)
str(d3.1)

as.numeric(d3.1$Year)
as.numeric(d3.1$Month)
as.numeric(d3.1$Day)

str(d3.1)
str(d2.1)
str(d1.1)

d4<-rbind(d3.1,d2.1,d1.1)


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
#So April through September is summer and October through March is winter

names(d4)

unique(d4$Year)
unique(d4$Month)



d4$Period <- NA
firstyear <- 2015 
#this is the first year of the FWC NFWF, so doing this to match the periods
endyear <- max(d4$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(d4)){
    if(d4$Year[j] == y & d4$Month[j] > 3 & d4$Month[j] < 10) d4$Period[j] = p[1] #year i months 4-9
    if(d4$Year[j] == y & d4$Month[j] > 9) d4$Period[j] = p[2] #year i months 10-12
    if(d4$Year[j] == y+1 & d4$Month[j] < 4) d4$Period[j] = p[2] #year i+1 months 1-3
  }
}

d4$Season <- "Winter"
d4$Season[d4$Period == 1 | d4$Period == 3 | d4$Period == 5 | d4$Period == 7 | d4$Period == 9| d4$Period == 11| d4$Period == 13] <- "Summer"

unique(d4$Period)
#periods 10  7  5 only

#ok what are our site names?
unique(d4$Site)
#not lots of sites

####
#leave names for now, no errors or inconsistencies like removing extra space

d4.1<-d4 %>%
   mutate(Site = replace(Site,Site == "East of Power Lines Ridge", "East Power Lines"))

d5<-d4.1

#ok let's now write d5 (st andrew) to a file and then that will be the file
#we merge with the others

write.table(d5, file = "20220324_StAndrews_NRDA_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")

