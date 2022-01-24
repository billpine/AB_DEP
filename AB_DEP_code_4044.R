##
##THIS IS 4044A
##

#Apalachicola Bay analyses of DEP data


#The database and meta data are here 
#https://dev.seacar.waterinstitute.usf.edu/

#To do integrate 4044A and 5007A 
#Project 4044 is the NRDA project and it used rock
#Project 5007 is the RESTORE project and it used shell
#Plot data for each size class over time (adult, seed, spat)
#Plot biomass overtime (usually listed as total weight)
#Will need to extract dates and then create periods
#Because not all years are the same will likely need to use covariates to try and #explain difference in year such as river discharge

#about line 112 I write a file that takes 4044 and exports it, that's the file i'll merge with 5007


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


d1 <- read.csv("~/GitHub/AB_DEP/Data_4044A_Final_clean.csv")

names(d1)
head(d1)

#subset the columns to the ones you want to work with
d2 <- d1 %>% 
  dplyr::select(Harvested, Bay, Site, Quadrat, Weight_kg, Adults_75mm, 
                Seed_26_74mm, Spat_0_25mm, Live, Dead, Month, Day, Year, full_year)

#rename headers  

names(d2)[]<-c("Date","Bay", "Site", "Quadrat", "Weight", "Legal", "Sublegal", "Spat", "Total_Live", "Total_Dead","Month", "Day","Year", "Full_year")

str(d2)

# d3 <- d2 %>%
#   mutate(Year = year(d2$Full_year),
#          Month = month(d2$Full_year),
#          Day = day(d2$Full_year))


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
#So April through September is summer and October through March is winter

unique(d2$Year)



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

d2$season <- "Winter"
d2$season[d2$Period == 1 | d2$Period == 3 | d2$Period == 5 | d2$Period == 7 | d2$Period == 9] <- "Summer"

unique(d2$Period)
#periods 1, 3, 6, 7 only

#ok what are our site names?
unique(d2$Site)
#lots of sites

####
#Fix some name errors or inconsistencies like removing extra space

d3<-d2

d3.1<-d3 %>%
  mutate(Site = replace(Site,Site == "Square Bar ", "Square Bar"))
d3.2<-d3.1 %>%
  mutate(Site = replace(Site,Site == "Cabbage Top ", "Cabbage Top"))
d3.3<-d3.2 %>%
  mutate(Site = replace(Site,Site == "Goose Point ", "Goose Point"))
d3.4<-d3.3 %>%
  mutate(Site = replace(Site,Site == "Bayou Flats ", "Bayou Flats"))
d3.5<-d3.4 %>%
  mutate(Site = replace(Site,Site == "Hotel Bar ", "Hotel Bar"))
d3.6<-d3.5 %>%
  mutate(Site = replace(Site,Site == "Trout Bayou  #1", "Trout Bayou #1"))
d3.7<-d3.6 %>%
  mutate(Site = replace(Site,Site == "Normans Bar Middle ", "Normans Bar Middle"))
d3.8<-d3.7 %>%
  mutate(Site = replace(Site,Site == "Normans Bar Middle ", "Normans Bar Middle"))
d3.9<-d3.8 %>%
  mutate(Site = replace(Site,Site == "Doyle Bayou ", "Doyle Bayou"))
d3.91<-d3.9 %>%
  mutate(Site = replace(Site,Site == "Little Gully ", "Little Gully"))
d3.92<-d3.91 %>%
  mutate(Site = replace(Site,Site == "Redfish Creek 2 ", "Redfish Creek 2"))

d3.93<-d3.92 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola ", "Apalachicola"))
d3.94<-d3.93 %>%
  filter(d3.93$Bay =="Apalachicola")
  


d4<-d3.94

unique(d4$Site)

#ok let's now write d4 to a file and then that will be the file
#we merge with the others

write.table(d4, file = "4044_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")


##just some summaries to see what is going on

names(d4)
str(d4)
as.numeric()

#change to numbers if needed
d4 %>% mutate_if(is.integer,as.numeric) %>% str(d4)


#max and mins
max(d4$Adults_75mm, na.rm=T)
min(d4$Adults_75mm, na.rm=T)

max(d3$Sublegal)
min(d3$Sublegal)

max(d4$Spat_0_25mm, na.rm=T)
min(d4$Spat_0_25mm, na.rm=T)



#count number of quadrats per period, site
month <- d4 %>%
  dplyr::group_by(Period, Site) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Period, Site)
names(month) <- c("Period", "Station Name",
                  "Number Quadrats")