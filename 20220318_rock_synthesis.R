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
  ggtitle("Weight per quadrat") +
  scale_y_log10()+
  xlab("Period") +
  ylab("Weight")
#facet_wrap(~Project)


#this is by study on one plot 
r2<-ggplot(data = d3, aes(Period, CPUE_Weight, color=Project)) +
  geom_point(size=3)

r2+scale_color_manual(values=c("black", "light blue", "red", "dark blue"))

ggsave("weight.pdf", width = 10, height = 10)  


######
#glmmTMB approach
######
######################

library(glmmTMB)
library(bbmle)

d3$Roundwt<-round(d3$Weight,0)


#just the rounded weights, not accounting for different # quads
r3<-ggplot(data = d3, aes(Period, Roundwt, color=Project)) +
  geom_point(size=3)



#this model is asking how period and project influence weight
#using NB2 formulation (most common)
tmb1 <- glmmTMB(Roundwt ~ Period + Project + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
summary(tmb1)


#interaction term for different slopes
tmb2 <- glmmTMB(Roundwt ~ Period * Project + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
summary(tmb2)


###
library(ggeffects)

new.dat1 = data.frame(Roundwt = d3$Roundwt,
                      Period = d3$Period,
                      Project = d3$Project,
                      Num_quads = log(d3$Num_quads))

new.tmb1 <- glmmTMB(Roundwt ~ Period + Project + offset(Num_quads), data = new.dat1, family="nbinom2") #converge
ggpredict(new.tmb1)
pred_tmb1 = ggpredict(new.tmb1, terms = c("Period", "Project", "Num_quads[1]"), type = c('fe')) #for all projects
plot(pred_tmb1, facet=TRUE, colors=c("red","black","blue","orange"), add.data=FALSE)
plot(pred_tmb1, facet=FALSE, colors=c("red","black","blue","orange"), add.data=FALSE)

####
new.tmb2 <- glmmTMB(Roundwt ~ Period * Project + offset(Num_quads), data = new.dat1, family="nbinom2") #converge
ggpredict(new.tmb2)
pred_tmb2 = ggpredict(new.tmb2, terms = c("Period", "Project", "Num_quads[1]"), type = c('fe')) #for all projects
plot(pred_tmb2, facet=TRUE, colors=c("red","black","blue","orange"), add.data=FALSE)

#if you run pred_tmb2 without the [1] it calculates for average
#number of quads which is like 4.27. So then I predicted in pred_tmb3
#for 5 quads and the overlay w/ data looks good for NFWF 1 but not
#other projects

pred_tmb3 = ggpredict(new.tmb2, terms = c("Period", "Project", "Num_quads[5]"), type = c('fe')) #for all projects
plot(pred_tmb3, facet=TRUE, colors=c("red","black","blue","orange"), add.data=TRUE)


#this is plotting for 1 quadrat. If you add the real data, that is for a lot of
#quadrats so that's why the predicted is WAY less than real


####


nfwf_pred<- subset(pred_tmb1, pred_tmb1$group == "NFWF_1")
pr1 = ggplot(nfwf_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("NFWF_1 Weight by Period") +
  geom_point(data = d3[d3$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))

NRDA_4044_pred<- subset(pred_tmb1, pred_tmb1$group == "NRDA_4044")
pr2 = ggplot(NRDA_4044_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("NRDA_4044 Weight by Period") +
  geom_point(data = d3[d3$Project == "NRDA_4044",], mapping = aes(Period, Roundwt), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))


#######

#subset data for each project, fit tmbglm, then predict, then separate plot
#for each project

unique(d3$Project)

dNFWF_1<-subset(d3,d3$Project =="NFWF_1")
dFWC_2021<-subset(d3,d3$Project =="FWC_2021")
dNRDA_4044<-subset(d3,d3$Project =="NRDA_4044")
dNRDA_5007<-subset(d3,d3$Project =="NRDA_5007")
#########NFWF_1 only#################
#NFWF_1 this is working
tmb_NFWF <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNFWF_1, family="nbinom2") #converge
summary(tmb_NFWF)


dNFWF_1.new = data.frame(Roundwt = dNFWF_1$Roundwt,
                      Period = dNFWF_1$Period,
                      Project = dNFWF_1$Project,
                      Num_quads = log(dNFWF_1$Num_quads))

dNFWF_1.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dNFWF_1.new, family="nbinom2") #converge
ggpredict(dNFWF_1.new.tmb1)
dNFWF_1_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads[5]"), type = c('fe')) #for all projects
plot(dNFWF_1_pred, facet=FALSE, colors=c("red"), add.data=TRUE)


#note this matches the data well if you predict for 5 quads
#but to compare projects just predict for 1 quad.
#to keep checking if this is working correctly, just make this same
#subset for each study and make unique plots.
#need to get jennifer way to work.  

#jennifer way
nfwf_pred<- subset(pred_tmb1, pred_tmb1$group == "NFWF_1")
pr1 = ggplot(nfwf_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("NFWF_1 Weight by Period") +
  geom_point(data = d3[d3$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))

