##
##THIS IS 5007AA
##

#Apalachicola Bay analyses of DEP data


#The database and meta data are here 
#https://dev.seacar.waterinstitute.usf.edu/programs/details/5007


#about line 112 I write a file that takes 5007 and exports it, that's the file i'll merge with 4044


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


d1 <- read.csv("~/Git/AB_DEP/Data_5007A_Final_cleaned.csv")

names(d1)
head(d1)

#subset the columns to the ones you want to work with
d2 <- d1 %>% 
  dplyr::select(Harvested, Site, Quadrat, Weight_kg, Total_Adults_75mm, 
                Total_Seed_26_74mm, Total_Spat_0_25mm, Total_Live, full_year)

#rename headers  

names(d2)[]<-c("Date", "Site", "Quadrat", "Weight", "Legal", "Sublegal", "Spat", "Total_Live", "Full_year")


d3 <- d2 %>%
  mutate(Year = year(d2$Full_year),
         Month = month(d2$Full_year),
         Day = day(d2$Full_year))


#let's create periods of time as we done in Lone Cabbage and just
#put the samples into those periods.  That way we can just work with period
# such as winter or summer


#######################
#add period to split the year like we do with Lone Cabbage
#So April through September is summer and October through March is winter

unique(d3$Year)



d3$Period <- NA
firstyear <- 2015 
#this is the first year of the FWC NFWF, so doing this to match the periods
endyear <- max(d3$Year)

years <- sort(rep(firstyear:endyear, times = 1, each = 2))

for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(d3)){
    if(d3$Year[j] == y & d3$Month[j] > 3 & d3$Month[j] < 10) d3$Period[j] = p[1] #year i months 4-9
    if(d3$Year[j] == y & d3$Month[j] > 9) d3$Period[j] = p[2] #year i months 10-12
    if(d3$Year[j] == y+1 & d3$Month[j] < 4) d3$Period[j] = p[2] #year i+1 months 1-3
  }
}

d3$Season <- "Winter"
d3$Season[d3$Period == 1 | d3$Period == 3 | d3$Period == 5 | d3$Period == 7 | d3$Period == 9] <- "Summer"

unique(d3$Period)
#periods 1, 3, 6, 7 only

#ok what are our site names?
unique(d3$Site)
#lots of issues here
#as an example there is "Cat Point " with a space after Point
# and "Cat Point" with a space not after Point

####
#Fix a bunch of name errors

d3.1<-d3 %>%
  mutate(Site = replace(Site,Site == "8 Mile ", "8 Mile"))
d3.2<-d3.1 %>%
  mutate(Site = replace(Site,Site == "Cat Point ", "Cat Point"))
d3.3<-d3.2 %>%
  mutate(Site = replace(Site,Site == "East Hole 1", "East Hole #1"))
d3.4<-d3.3 %>%
  mutate(Site = replace(Site,Site == "East Hole 2", "East Hole #2"))

d4<-d3.4

unique(d4$Site)

#ok let's now write d4 to a file and then that will be the file
#we merge with 4044

write.table(d4, file = "5007_to_merge.csv", row.names = FALSE,col.names = TRUE,sep = ",")




#max and mins
max(d3$Legal)
min(d3$Legal)

max(d3$Sublegal)
min(d3$Sublegal)

max(d3$Spat)
min(d3$Spat)



#count number of quadrats per period, site
month <- d4 %>%
  dplyr::group_by(Period, Site) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Period, Site)
names(month) <- c("Period", "Station Name",
                  "Number Quadrats")


#just writing the table with number of quadrats by Period, site to folder
#write.table(month, file = "num_quads_yr_mnth_station.txt", row.names = FALSE,
#            col.names = TRUE,sep = ",")


report_data <- d4 %>%
  dplyr::group_by(Period,Year, Month, Site) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(Period,Year, Month, Site)
names(report_data) <- c("Period","Year", "Month", "Site",
                  "Number Quadrats")
write.table(month, file = "num_quads_yr_mnth_site.txt", row.names = FALSE,col.names = TRUE,sep = ",")




f1<-ggplot(d4, aes(Period, Legal, color=Site)) +
  geom_point(size=4) +
  ggtitle("Legal by Period") +
  xlab("Period") +
  ylab("Number legal") #+
  #stat_summary(fun = mean, geom = "point", size=1.5, aes(group= Site), color="gold") +
  #stat_summary(fun.data = "mean_cl_boot",aes(group= Site), size = 1.5, geom = "errorbar", width = 0.5, color="gold")
f1

f2<-ggplot(d4, aes(Period, Sublegal, color=Site)) +
  geom_point(size=4) +
  ggtitle("Sublegal by Period") +
  xlab("Period") +
  ylab("Number Sublegal") #+
#stat_summary(fun = mean, geom = "point", size=1.5, aes(group= Site), color="gold") +
#stat_summary(fun.data = "mean_cl_boot",aes(group= Site), size = 1.5, geom = "errorbar", width = 0.5, color="gold")

f2

#
#sum live counts for each site by period
sum_legal=aggregate(Legal~Site+Period+Season,data=d4,sum)
sum_sublegal=aggregate(Sublegal~Site+Period+Season,data=d4,sum)
sum_spat=aggregate(Spat~Site+Period+Season,data=d4,sum)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Quadrat~Site+Period+Season,data=d3,length)
count_quads <- dplyr::rename(count_quads,Site=Site,Num_quads=Quadrat, Period=Period,Season=Season)


#merge live count total data frame with the tran_length total data frame
d5=merge(sum_legal,sum_sublegal,by=c("Site","Period","Season"))
d5.1=merge(d5,sum_spat,by=c("Site","Period","Season"))
d5.2=merge(d5.1,count_quads,by=c("Site","Period","Season"))

d6<-d5.2

##add substrate type
d6$Substrate<-"rock"

names(d6)

#plot
f3<-ggplot(d6, aes(x=Period, y= Legal)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("Legal oysters") +
  #ylim(0,300)+
  xlab("Period") +
  ylab("Number legal oysters") +
  facet_wrap(~Site)
f3
f3.1<-ggplot(d6, aes(x=Period, y= Sublegal)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("SubLegal oysters") +
  #ylim(0,300)+
  xlab("Period") +
  ylab("Number Sublegal oysters") +
  facet_wrap(~Site)
f3.1

f3.2<-ggplot(d6, aes(x=Period, y= Spat)) +
  geom_point(size=3.5, alpha =1) +
  ggtitle("SubLegal oysters") +
  #ylim(0,300)+
  xlab("Period") +
  ylab("Number Spat oysters") +
  facet_wrap(~Site)
f3.2

##########
###GLMs###
##########

d6$StationName <- as.factor(d5$StationName)
d6$Season <- as.factor(d5$Season)

#fit basic NB GLM
m1 <- glm.nb(Legal ~ Period + offset(log(Num_quads)), data = d6) 
m2 <- glm.nb(Legal ~ Period + Site + offset(log(Num_quads)), data = d6) 
m3 <- glm.nb(Legal ~ Period * Site + offset(log(Num_quads)), data = d6,control = glm.control(maxit = 5000)) 


cand.set = list(m1,m2,m3)
modnames = c("period", "period + site", "period * site",)
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

##############
#summary stats
##############

###################
#from oyster weekly report

options(scipen = 2)
sumstats = function(x){ 
  y=na.omit(x)
  bstrap <- c()
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(y,(length(y)),replace=T), na.rm = T))}
  c(
    Mean=mean(y), 
    Median=median(y),
    SD=sd(y), 
    Var=var(y),
    CV=sd(y)/mean(y),
    SE=sd(y)/sqrt(length(y)),
    L95SE=mean(y)-1.96*(sd(y)/sqrt(length(y))),
    U95SE=mean(y)+1.96*(sd(y)/sqrt(length(y))),
    BSMEAN = mean(bstrap),
    L95BS = quantile(bstrap,.025),
    U95BS= quantile(bstrap,.975))
}

a<-round(sumstats(d4$Spat[d4$Site == "Cat Point" & d4$Period == "7" ]),2)
write.table(a, file = "hotel_p2.txt", row.names = TRUE,
            col.names = TRUE,sep = ",")




