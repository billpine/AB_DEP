#Apalachicola oyster data from two DEP files and 1 FWC file 
#data from quadrats
#Bill Pine
#January 2022

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

d1 <- read.csv("~/Git/AB_DEP/20220126_merged_agency_data.csv")

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Sublegal[d2$Sublegal < -1] <- NA
d2$Legal[d2$Legal < -1] <- NA

#make summary table of number of quadrats each month and period

quad_sum<- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::count(Project, Year, Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Project, Year, Month, Period, Site)
names(quad_sum) <- c("Project", "Year", "Month","Period", "Site",
                  "Number_Quadrats")

#just writing the table with number of quadrats by year, month, station to folder
write.table(quad_sum, file = "quad_count_project_yr_mnth_station.txt", row.names = FALSE,
            col.names = TRUE,sep = ",")

##summary number of oyster spat counted each month
#by Project, Year, Month, Period, Site
spat_sum <- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::summarise(sum=sum(Spat,na.rm=TRUE)) %>%
  dplyr::arrange(Project, Year, Month, Period, Site)

  names(spat_sum) <- c("Project", "Year", "Month","Period", "Site",
                     "Number Live Spat")

  write.table(spat_sum, file = "spat_count_yr_mnth_station.txt", row.names = FALSE,
              col.names = TRUE,sep = ",")
  
  
# #OK parking the summary stats function here
# options(scipen = 2)
# sumstats = function(x){ 
#   y=na.omit(x)
#   bstrap <- c()
#   for (i in 1:1000){
#     bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
#   c(
#     Mean=mean(y), 
#     Median=median(y),
#     SD=sd(y), 
#     Var=var(y),
#     CV=sd(y)/mean(y),
#     SE=sd(y)/sqrt(length(y)),
#     L95SE=mean(y)-1.96*(sd(y)/sqrt(length(y))),
#     U95SE=mean(y)+1.96*(sd(y)/sqrt(length(y))),
#     BSMEAN = mean(bstrap),
#     L95BS = quantile(bstrap,.025),
#     U95BS= quantile(bstrap,.975))
# }
# 
# a<-round(sumstats(d2$Spat[d2$Site == "Dry Bar" & d2$Period == "2" ]),2)
# write.table(a, file = "dry_p2.txt", row.names = TRUE,
#             col.names = TRUE,sep = ",")
# 
# 
# b<-round(sumstats(d2$Spat[d2$Site == "Dry Bar" & d2$Period == "12" ]),2)
# write.table(b, file = "dry_p13.txt", row.names = TRUE,
#             col.names = TRUE,sep = ",")
# ###

#sum live counts for each transect
count_spat=aggregate(Spat~Site+Period+Season,data=d2,sum)
count_spat <- dplyr::rename(count_spat,Site=Site,Period=Period, Season=Season,Sum_spat=Spat)

count_legal=aggregate(Legal~Site+Period+Season,data=d2,sum)
count_legal <- dplyr::rename(count_legal,Site=Site,Period=Period, Season=Season,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Spat~Site+Period+Season,data=d2,length)
count_quads_spat <- dplyr::rename(count_quads,Site=Site,Period=Period, Season=Season,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d3=merge(count_spat,count_quads_spat,by=c("Site", "Period", "Season"))

#calculate CPUE. Just for fun to plot
d3$CPUE<-d3$Sum_spat/d3$Num_quads

plot(d3$Period,d3$CPUE)

dis_quant1 <- subset(dis_quant, dis_quant$quantile != '0')

CPUE_Cat<-subset(d3,d3$Site =="Cat Point")
CPUE_Hotel<-subset(d3,d3$Site =="Hotel Bar")
CPUE_Dry<-subset(d3,d3$Site =="Dry Bar")
CPUE_Bulkhead<-subset(d3,d3$Site =="Bulkhead")

f1<-ggplot(CPUE_Cat, aes(Period, CPUE)) +
  geom_point(size=4) +
  ggtitle("Cat Point") +
  xlim(0,15)+
  xlab("Period") +
  ylab("Spat CPUE")

f2<-ggplot(CPUE_Hotel, aes(Period, CPUE)) +
  geom_point(size=4) +
  ggtitle("Hotel Bar") +
  xlim(0,15)+
  xlab("Period") +
  ylab("Spat CPUE")

f3<-ggplot(CPUE_Dry, aes(Period, CPUE)) +
  geom_point(size=4) +
  xlim(0,15)+
  ggtitle("Dry Bar") +
  xlab("Period") +
  ylab("Spat CPUE")

f4<-ggplot(CPUE_Bulkhead, aes(Period, CPUE)) +
  geom_point(size=4) +
  xlim(0,15)+
  ggtitle("Bulkhead") +
  xlab("Period") +
  ylab("Spat CPUE")


plot_grid(f1,f2,f3,f4)
###

#everything below is just on the bench and does not work


# 
# #some box plots
# 
# p1<-ggplot(data=d2) +
#   labs(title="Counts of live spat") + 
#   geom_boxplot(
#     mapping = aes(
#       x=Site,
#       y=Spat))
# p1
# 
# p1.1<-p1+  
#   facet_grid(Year~.) +
#   labs(title = "Boxplot by Year & Site")
# p1.1
# 
# p2<-ggplot(data=d2) +
#   labs(title="Counts of live spat") + 
#   aes(x=Spat,
#       color=Site)+
#   stat_density(aes(group = Site), position="stack",geom="line", 
#                size= 1.5)
# p2
# 
# p2.1<-p2+  
#   facet_grid(Period~.) +
#   labs(title = "PDF live spat by period")
# p2.1
# 
# #by period & station
# summary1<-d2%>%
#   group_by(Period,Site)%>%
#   summarise(mean=mean(Spat,na.rm=TRUE),
#             std_dev=sd(Spat, na.rm=TRUE))
# 
# 
# 
# 
# f1<-ggplot(d2, aes(Period, Spat)) +
#   geom_point(size=4) +
#   ggtitle("Live Spat by Period") +
#   xlab("Period") +
#   ylab("Live Spat") +
#   facet_wrap(Project)
#   
#   stat_summary(fun = mean, geom = "point", size=1.5, aes(group= Period), color="gold") +
#   stat_summary(fun.data = "mean_cl_boot",aes(group= Period), size = 1.5, geom = "errorbar", width = 0.5, color="gold")
# 
# f1
# 
# 
# 
# 
# 
# 
# 
# names(d1)
# as.numeric(d1$Period)
# as.numeric(d1$Spat)
# 
# #plot
# f1<-ggplot(d1, aes(x=Period, y= Spat, color=Bottom)) +
#   geom_point(size=3.5, alpha =1)
#   facet_wrap(d1$Project)
# 
# 
#   ggtitle("Live Spat")
# #+
#   #ylim(0,30000)+
#   xlab("Period") +
#   ylab("Live oyster spat")+
#   facet_wrap(~Project)
# f1
# 
