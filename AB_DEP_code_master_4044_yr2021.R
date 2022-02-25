##
##THIS IS the DEP project 4044 master data file
##I received this from DEP jan 25, 2022 after I cleaned and standardized
#the 4044 and 5007 files from the SECAR database
#so now I"m going to clean just 2021 from the master file
#because that year is not in SECAR
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
d1 <- read_excel("ApalachicolaNRDAMonitoringData_MASTER.xlsx", 
                 sheet = "Round 4 Oyster Counts", col_types = 
                   c("text","date", "date", "text", "text", "text", 
                     "text", "text", "text", "text", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "numeric", "numeric", "numeric", 
                     "text"))

names(d1)
head(d1)

d1$Date<-as.POSIXct(d1$Harvested, format="%Y-%m-%d")

d1$Year<-format(d1$Date,format="%Y")
d1$Month<-format(d1$Date,format="%m")
d1$Day<-format(d1$Date,format="%d")

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
d2 <- d1 %>% 
  dplyr::select("Bay","Site","Quadrat","Weight_kg","Adults"
                ,"Seed","Spat","Total Live",
                "Total Dead","Month","Day","Year","Date")


d2$Year<-as.numeric(d2$Year)
d2$Month<-as.numeric(d2$Month)
d2$Day<-as.numeric(d2$Day)

names(d2)

str(d2)


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
#So April through September is summer and October through March is winter

names(d2)

unique(d2$Year)
unique(d2$Month)



d2$Period <- NA
firstyear <- 2015 
#this is the first year of the FWC NFWF, so doing this to match the periods
endyear <- max(d2$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(d2)){
    if(d2$Year[j] == y & d2$Month[j] > 3 & d2$Month[j] < 10) d2$Period[j] = p[1] #year i months 4-9
    if(d2$Year[j] == y & d2$Month[j] > 9) d2$Period[j] = p[2] #year i months 10-12
    if(d2$Year[j] == y+1 & d2$Month[j] < 4) d2$Period[j] = p[2] #year i+1 months 1-3
  }
}

d2$Season <- "Winter"
d2$Season[d2$Period == 1 | d2$Period == 3 | d2$Period == 5 | d2$Period == 7 | d2$Period == 9| d2$Period == 11| d2$Period == 13] <- "Summer"

unique(d2$Period)
#periods 1, 3, 6, 7 only

#ok what are our site names?
unique(d2$Site)
#lots of sites

####
#Fix some name errors or inconsistencies like removing extra space

#note names in this file look ok, but I'll leave this code
#here as an example on how to clean it up if we go to strata or errors observed
#later

d3<-d2

# d3.1<-d3 %>%
#   mutate(Site = replace(Site,Site == "Square Bar ", "Square Bar"))
# d3.2<-d3.1 %>%
#   mutate(Site = replace(Site,Site == "Cabbage Top ", "Cabbage Top"))


unique(d3$Site)

#ok let's now write d4 to a file and then that will be the file
#we merge with the others

write.table(d3, file = "4044_yr2021_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")

# 
# ##just some summaries to see what is going on
# 
# names(d3)
# str(d3)
# as.numeric()
# 
# #change to numbers if needed
# d3 %>% mutate_if(is.integer,as.numeric) %>% str(d3)
# 
# 
# #max and mins
# max(d4$Adults_75mm, na.rm=T)
# min(d4$Adults_75mm, na.rm=T)
# 
# max(d3$Sublegal)
# min(d3$Sublegal)
# 
# max(d4$Spat_0_25mm, na.rm=T)
# min(d4$Spat_0_25mm, na.rm=T)
# 
# 
# 
# #count number of quadrats per period, site
# month <- d4 %>%
#   dplyr::group_by(Period, Site) %>%
#   dplyr::summarise(count = n()) %>%
#   dplyr::arrange(Period, Site)
# names(month) <- c("Period", "Station Name",
#                   "Number Quadrats")