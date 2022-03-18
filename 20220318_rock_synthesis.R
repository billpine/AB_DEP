#Apalachicola oyster data from three DEP files and 2 FWC file 

###REMEMBER THESE ARE ONLY APALACHICOLA DATA

#data from quadrats
#Bill Pine
#March 2022

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

d1 <- read.csv("~/Git/AB_DEP/20220305_merged_agency_data.csv")



#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Sublegal[d2$Sublegal < -1] <- NA
d2$Legal[d2$Legal < -1] <- NA

data_sum<- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::count(Project, Year,Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Project, Year, Month, Period, Site)
names(data_sum) <- c("Project", "Year", "Month","Period", "Site",
                     "Number_Quadrats")



#make summary table of number of quadrats each month and period

quad_sum<- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::count(Project, Year, Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Project, Year, Month, Period, Site)
names(quad_sum) <- c("Project", "Year", "Month","Period", "Site",
                     "Number_Quadrats")

#just writing the table with number of quadrats by year, month, station to folder
#write.table(quad_sum, file = "quad_count_project_yr_mnth_station.txt", row.names = FALSE,
#            col.names = TRUE,sep = ",")

##sum cultch weight each month
#by Project, Year, Month, Period, Site
wt_sum <- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::summarise(sum=sum(Weight,na.rm=TRUE)) %>%
  dplyr::arrange(Project, Year, Month, Period, Site)

names(wt_sum) <- c("Project", "Year", "Month","Period", "Site",
                     "Weight")

#sum wt for each site period season
wt=aggregate(Weight~Project+Site+Period+Season,data=d2,sum)
sum_wt <- dplyr::rename(wt,Project=Project, Site=Site,Period=Period, Season=Season,Weight=Weight)


#count number quads by doing the length of transect, then rename
count_quads=aggregate(Weight~Project+Site+Period+Season,data=d2,length)
count_quads_weight <- dplyr::rename(count_quads,Site=Site,Period=Period, Season=Season,Num_quads=Weight)

#merge weight total data frame with the tran_length total data frame
d3=merge(sum_wt,count_quads_weight,by=c("Project","Site", "Period", "Season"))



#calculate CPUE. Just for fun to plot
d3$CPUE_Weight<-d3$Weight/d3$Num_quads


CPUE_FWC_2021<-subset(d3,d3$Project =="FWC_2021")
CPUE_NFWF_1<-subset(d3,d3$Site =="NFWF_1")
CPUE_NRDA_4044<-subset(d3,d3$Site =="NRDA_4044")
CPUE_NRDA_5007<-subset(d3,d3$Site =="NRDA_5007")

f1<-ggplot(CPUE_FWC_2021, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Cat Point Legal") +
  xlim(0,14)+
  xlab("Period") +
  ylab("CPUE Legal")

#this is by study on one plot 
r1<-ggplot(data = d3[d3$Project=="NFWF_1",], aes(Period, CPUE_Weight, color="NFWF_1")) +
  geom_point(size=3) +
  geom_point(data = d3[d3$Project=="NRDA_4044",], mapping = aes(Period, CPUE_Weight, color="NRDA_4044"), size = 3)+
  geom_point(data = d3[d3$Project=="NRDA_5007",], mapping = aes(Period, CPUE_Weight, color="NRDA_5077"), size = 3)+
  geom_point(data = d3[d3$Project=="FWC_2021",], mapping = aes(Period, CPUE_Weight, color="FWC_2021"), size = 3)+
  
  ggtitle("Spat per Period by Study") +
  scale_y_log10()+
  xlab("Period") +
  ylab("Total Spat")
#facet_wrap(~Project)


#this is by study on one plot 
r2<-ggplot(data = d3, aes(Period, CPUE_Weight, color=Project)) +
  geom_point(size=3)

r2+scale_color_manual(values=c("black", "light blue", "red", "dark blue"))

  


f2<-ggplot(CPUE_Hotel, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Hotel Bar Legal") +
  xlim(0,14)+
  xlab("Period") +
  ylab("CPUE Legal")

f3<-ggplot(CPUE_Dry, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  xlim(0,14)+
  ggtitle("Dry Bar Legal") +
  xlab("Period") +
  ylab("CPUE Legal")

f4<-ggplot(CPUE_Bulkhead, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  xlim(0,14)+
  ggtitle("Bulkhead") +
  xlab("Period") +
  ylab("CPUE Legal")


