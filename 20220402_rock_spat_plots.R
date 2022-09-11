#Apalachicola oyster data from three DEP files and 2 FWC file 



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


#if you want to only do the rock analyses with Apalach then you can
#subset for Apalach here. Else make d1 the full data file with all bays
#d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")
d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")

#ok, change Apalachicola Bay to Apalachicola
d0.1<-d0 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))%>%
  mutate(Project = replace(Project,Project =="FWC-2021","NFWF-2021"))

d1<- d0.1

#d1<- subset(d0.1, d0.1$Bay == "Apalachicola")

#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Weight_kg[d2$Weight_kg < -1] <- NA
d2$Seed[d2$Seed < -1] <- NA
d2$Legal[d2$Legal < -1] <- NA

data_sum<- d2 %>%
  dplyr::group_by(Bay, Project, Year, Month, Period, Site) %>%
  dplyr::count(Bay, Project, Year, Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Bay, Project, Year, Month, Period, Site)
names(data_sum) <- c("Bay", "Project", "Year", "Month","Period", "Site",
                     "Number_Quadrats")

#make summary table of number of quadrats each month and period

quad_sum<- d2 %>%
  dplyr::group_by(Bay, Project, Year, Month, Period, Site) %>%
  dplyr::count(Bay, Project, Year, Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Bay, Project, Year, Month, Period, Site)
names(quad_sum) <- c("Bay", "Project", "Year", "Month","Period", "Site",
                     "Number_Quadrats")
dx<-quad_sum %>%
  arrange(Bay, Period, Project, Year, Month, Site)

quad_sumx<- d2 %>%
  dplyr::group_by(Bay, Project, Period, Year, Month) %>%
  dplyr::count(Bay, Project, Period, Year, Month)


#just writing the table with number of quadrats by year, month, station to folder
#write.table(dx, file = "quad_count_project_yr_mnth_station.txt", row.names = FALSE,
#            col.names = TRUE,sep = ",")

##sum cultch weight each month
#by Project, Year, Month, Period, Site

#mean wt for each site period season
wt=aggregate(Weight~Bay+Project+Site+Period,data=d2,mean)
mean_wt <- dplyr::rename(wt, Bay=Bay,Project=Project, Site=Site,Period=Period, Weight=Weight)
names(mean_wt) <- c("Bay", "Project", "Site", "Period", "Wt_mean")

#sum spat for each site period season
spat=aggregate(Spat~Bay+Project+Site+Period,data=d2,mean)
mean_spat <- dplyr::rename(spat, Bay=Bay,Project=Project, Site=Site,Period=Period, Mean_Spat=Spat)
names(mean_spat) <- c("Bay","Project", "Site", "Period", "Spat_mean")

#count number quads by doing the length of aggregated quads, then rename
count_quads=aggregate(Weight~Bay+Project+Site+Period,data=d2,length)
count_quads_weight <- dplyr::rename(count_quads,Site=Site,Period=Period, Num_quads=Weight)

#merge weight total data frame with the num quads total data frame and spat total
d2.9=merge(mean_wt, mean_spat,by=c("Bay", "Project","Site", "Period"))
#merge weight total data frame with the num quads total data frame and spat total
d3=merge(d2.9, count_quads_weight,by=c("Bay", "Project","Site", "Period"))


#this is by study on one plot, then wrapped by Bay 
r1<-ggplot(data = d3[d3$Bay=="Apalachicola",], aes(x=Wt_mean, y=Spat_mean, color=Project)) +
  geom_point(size=3)+
  #ggtitle("Apalachicola Mean Cultch Weight vs. Mean Number Spat Per Quad") +
  xlab("Mean weight (kg)") +
  ylab("Mean spat")+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  facet_wrap(~Site)

#ggsave("meanwt_meanspat.png", width=10, height=10)



###ok now just plot the raw spat and raw weights, not the mean
names(d2)

r2<-ggplot(data = d2[d2$Bay=="Apalachicola",], aes(x=Weight, y=Spat, color=Project, na.rm=TRUE)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  xlab("Cultch weight (kg)") +
  ylab("Spat")+
  facet_wrap(~Site)
ggsave("r2_rawspat_rawweight_site.png", width=10, height=10)


r3<-ggplot(data = d2[d2$Bay=="Apalachicola",], aes(x=Weight, y=Spat, color=Project, na.rm=TRUE)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  xlab("Cultch weight (kg)") +
  ylab("Spat")+
  facet_wrap(~Period)
ggsave("r3_rawspat_rawweight_period.png", width=10, height=10)

  
plot_grid(r2, r3, labels = c('A', 'B'))

ggsave("rawwt_rawspat_site_period.png", width=10, height=10)
