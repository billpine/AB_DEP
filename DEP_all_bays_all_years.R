
#this is the DEP data from project 4044
#but for all three bays (pensacola, east bay, apalach)

d1 <- read.csv("~/Git/AB_DEP/DEP_three_bays_4044.csv")

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

names(d1)
head(d1)

names(d1)[16] <- "Season"
names(d1)

#ID the project

d1$Project<-"NRDA_4044"
d1$Bottom<-"Shell"
d1$Cultch<-200

#subset the columns to the ones you want to work with

d2 <- d1 %>% 
  dplyr::select(Project, Bay, Site, Quadrat, Weight, Legal, 
                Sublegal, Spat, Month, Day, Year, Period, Season, Bottom, Cultch)

str(d2)
names(d2)


############
#now bring in 2021 for this project
#which was provided by DEP (Jonathan) separately
#now need to merge d4 which is project 4044 year = 2021
# to the other years of 4044

d3 <- read.csv("~/Git/AB_DEP/DEP_Apalach_4044_yr2021.csv")

d3$Project<-"NRDA_4044"
d3$Bay<-"Apalachicola"

names(d2)
names(d3)

str(d2)
str(d3)

#merge these files together
d4<-rbind(d2, d3)

names(d4)
#check bays
unique(d4$Bay)
#check sites
unique(d4$Site)

min(d4$Legal)
min(d4$Sublegal)
min(d4$Spat)
######


#count number of quadrats per period, site
quad_sum<- d4 %>%
  dplyr::group_by(Period, Bay) %>%
  dplyr::count(Period, Bay) %>%
  dplyr::relocate(Period, Bay)
names(quad_sum) <- c("Period", "Bay",
                     "Number_Quadrats")

##summary number of oyster spat counted each period
spat_sum <- d4 %>%
  dplyr::group_by(Bay, Period) %>%
  dplyr::summarise(sum=sum(Spat,na.rm=TRUE)) %>%
  dplyr::arrange(Bay, Period)

names(spat_sum) <- c("Bay","Period",
                     "Number Live Spat")

#sum live counts for each transect
count_spat=aggregate(Spat~Bay+Period,data=d4,sum)
count_spat <- dplyr::rename(count_spat,Bay=Bay,Period=Period,Sum_spat=Spat)

count_sublegal=aggregate(Sublegal~Bay+Period,data=d2,sum)
count_sublegal <- dplyr::rename(count_sublegal,Bay=Bay,Period=Period,Sum_sublegal=Sublegal)

count_legal=aggregate(Legal~Bay+Period,data=d2,sum)
count_legal <- dplyr::rename(count_legal,Bay=Bay,Period=Period,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Spat~Bay+Period,data=d2,length)
count_quads_spat <- dplyr::rename(count_quads,Bay=Bay,Period=Period,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d3=merge(count_spat,count_quads_spat,by=c("Bay", "Period"))
d3.1=merge(d3,count_sublegal,by=c("Bay", "Period"))
d3.2=merge(d3.1,count_legal,by=c("Bay", "Period"))

d3<-d3.2

#calculate CPUE. Just for fun to plot
d3$CPUE_Spat<-d3$Sum_spat/d3$Num_quads
d3$CPUE_Sublegal<-d3$Sum_sublegal/d3$Num_quads
d3$CPUE_Legal<-d3$Sum_legal/d3$Num_quads

f1<-ggplot(d3, aes(Period, CPUE_Spat)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

f2<-ggplot(d3, aes(Period, CPUE_Sublegal)) +
  geom_point(size=4) +
  ggtitle("Sublegal CPUE by Period") +
  xlab("Period") +
  ylab("Sublegal") +
  facet_wrap(~Bay)

f3<-ggplot(d3, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(f1,f2,f3)
