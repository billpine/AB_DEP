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
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))

#d1<- d0.1

d1<- subset(d0.1, d0.1$Bay == "Apalachicola")

#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Sublegal[d2$Sublegal < -1] <- NA
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
wt_sum <- d2 %>%
  dplyr::group_by(Bay, Project, Year, Month, Period, Site) %>%
  dplyr::summarise(sum=sum(Weight,na.rm=TRUE)) %>%
  dplyr::arrange(Bay, Project, Year, Month, Period, Site)

names(wt_sum) <- c("Bay", "Project", "Year", "Month","Period", "Site",
                     "Weight")

#sum wt for each site period season
wt=aggregate(Weight~Bay+Project+Site+Period+Season,data=d2,sum)
sum_wt <- dplyr::rename(wt, Bay=Bay,Project=Project, Site=Site,Period=Period, Season=Season,Weight=Weight)

#count number quads by doing the length of aggregated quads, then rename
count_quads=aggregate(Weight~Bay+Project+Site+Period+Season,data=d2,length)
count_quads_weight <- dplyr::rename(count_quads,Site=Site,Period=Period, Season=Season,Num_quads=Weight)

#merge weight total data frame with the num quads total data frame
d3=merge(sum_wt,count_quads_weight,by=c("Bay", "Project","Site", "Period", "Season"))

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

#this is by study on one plot, then wrapped by Bay 
r1<-ggplot(data = d3[d3$Project=="NFWF_1",], aes(Period, CPUE_Weight, color="NFWF_1")) +
  geom_point(size=3) +
  geom_point(data = d3[d3$Project=="NRDA_4044",], mapping = aes(Period, CPUE_Weight, color="NRDA_4044"), size = 3)+
  geom_point(data = d3[d3$Project=="NRDA_5007",], mapping = aes(Period, CPUE_Weight, color="NRDA_5077"), size = 3)+
  geom_point(data = d3[d3$Project=="FWC_2021",], mapping = aes(Period, CPUE_Weight, color="FWC_2021"), size = 3)+
  ggtitle("Weight per quadrat") +
  #scale_y_log10()+
  xlab("Period") +
  ylab("Weight")+
facet_wrap(~Bay)

#this is by study on one plot  (all bays combined)
r2<-ggplot(data = d3, aes(Period, CPUE_Weight, color=Project)) +
  geom_point(size=3)

r2+scale_color_manual(values=c("black", "light blue", "red", "orange"))+
  facet_wrap(~Bay)

#ggsave("weight.pdf", width = 10, height = 10)  


######
#glmmTMB approach
######
######################

library(glmmTMB)
library(bbmle)

d3$Roundwt<-round(d3$Weight,0)

#just the rounded weights, not accounting for different # quads
r3<-ggplot(data = d3, aes(Period, Roundwt, color=Project)) +
  geom_point(size=3)+
  facet_wrap(~Bay)

#subset data for each project, fit tmbglm, then predict, then separate plot
#for each project. This is to check on intercepts to make sure
#when you go to the interaction term model the interpretation
#is easier because you can compare back to this simpler model

unique(d3$Project)

dNFWF_1<-subset(d3,d3$Project =="NFWF_1") #only Apalach Bay
dFWC_2021<-subset(d3,d3$Project =="FWC_2021") #only Apalach Bay
dNRDA_4044<-subset(d3,d3$Project =="NRDA_4044") #Pensacola, St. Andrew, Apalach bay
dNRDA_5007<-subset(d3,d3$Project =="NRDA_5007") #only Apalach Bay
dPeriod<-2:13 

#I'm adding dPeriod to have it predict over all periods
#even if sampling was not done all periods. just for graphing
#but I'm not sure this is the best way to do this forcing predictions
#over periods not sampled


#########NFWF_1 only#################
#NFWF_1 this is working
tmb_NFWF <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNFWF_1, family="nbinom2") #converge
summary(tmb_NFWF)


dNFWF_1.new = data.frame(Roundwt = dNFWF_1$Roundwt,
                         Period = dPeriod,
                         Project = dNFWF_1$Project,
                         Num_quads = dNFWF_1$Num_quads)

dNFWF_1.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(log(Num_quads)), data = dNFWF_1.new, family="nbinom2") #converge
ggpredict(dNFWF_1.new.tmb1)


#below is not specifying # quads so it just picks 3 values near the average
dNFWF_1_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads[150]"), type = c('fe')) #for all projects
plot(dNFWF_1_pred, facet=FALSE, add.data=TRUE)

#the above is a good demonstration the model is a decent fit to data

#this predicts with 1 quad all periods which is how we will compare the 3 projects
dNFWF_2_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
plot(dNFWF_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)

#note this matches the data well if you predict for 5 quads
#but to compare projects just predict for 1 quad.
#to keep checking if this is working correctly, just make this same
#subset for each study and make unique plots.

#jennifer way
pr1 = ggplot(dNFWF_2_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("Predicted NFWF_1 Cultch (Shell) Weight by Period - 1 quadrat")+
  #scale_x_continuous(breaks=seq(2,13,1))+
  ylim(0,10)
  
#no need to include the data on single quadrat because the data are from
#more than 1 quadrat
# +
#   geom_point(data = d3[d3$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))

#########FWC_2021 only#################
#this one is tricky because it is only a few years

tmb_FWC_2021 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dFWC_2021, family="nbinom2") #converge
summary(tmb_FWC_2021)

dFWC_2021.new = data.frame(Roundwt = dFWC_2021$Roundwt,
                           Period = dFWC_2021$Period,
                           Project = dFWC_2021$Project,
                           Num_quads = dFWC_2021$Num_quads)

dFWC_2021.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(log(Num_quads)), data = dFWC_2021.new, family="nbinom2") #converge
ggpredict(dFWC_2021.new.tmb1)


#below is not specifying # quads so it just picks 3 values from 1.5 to 3.5
dFWC_2021_pred = ggpredict(dFWC_2021.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
plot(dFWC_2021_pred, facet=FALSE, add.data=TRUE)

#the above is a good demonstration the model is a decent fit to data

#this predicts with 1 quad which is how we will compare the 3 projects
dFWC_2_pred = ggpredict(dFWC_2021.new.tmb1, terms = c("Period[2:13]", "Num_quads[1]"), type = c('fe')) #for all projects
plot(dFWC_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)

#note this matches the data well if you predict for 1-3 quads
#but to compare projects just predict for 1 quad.
#to keep checking if this is working correctly, just make this same
#subset for each study and make unique plots.

#jennifer way
pr2 = ggplot(dFWC_2_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("Predicted FWC_2021 Cultch (Rock) Weight by Period - 1 quadrat") +
  #scale_x_continuous(breaks=seq(2,13,1))+
  ylim(0,10)
  
#no need to include the data on single quadrat plots because the data are from
#more than 1 quadrat
# +
#   geom_point(data = d3[d3$Project == "FWC_2021",], mapping = aes(Period, Roundwt), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))

#########NRDA 4044 only#################
##REMEMBER THIS PROJECT IS ALL THE BAYS
#Pensacola and St. Andrews are rock
#Apalachicola is shell

#you could subset in this section the data data = dNRDA_4044
#to be only Bay = Apalach if you wanted


tmb_NRDA_4044 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNRDA_4044, family="nbinom2") #converge
summary(tmb_NRDA_4044)

dNRDA_4044.new = data.frame(Roundwt = dNRDA_4044$Roundwt,
                            Period = dNRDA_4044$Period,
                            Project = dNRDA_4044$Project,
                            Num_quads = dNRDA_4044$Num_quads)

dNRDA_4044.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(log(Num_quads)), data = dNRDA_4044.new, family="nbinom2") #converge
ggpredict(dNRDA_4044.new.tmb1)


#below is not specifying # quads so it just picks 3 values from 2 to 4.5
dNRDA_4044_pred = ggpredict(dNRDA_4044.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
plot(dNRDA_4044_pred, facet=FALSE, add.data=TRUE)

#the above is a good demonstration the model is a decent fit to data

#this predicts with 1 quad which is how we will compare the 3 projects
dNRDA_4044_2_pred = ggpredict(dNRDA_4044.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
plot(dNRDA_4044_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)

#note this matches the data well if you predict for 1-3 quads
#but to compare projects just predict for 1 quad.
#to keep checking if this is working correctly, just make this same
#subset for each study and make unique plots.

#jennifer way
pr3 = ggplot(dNRDA_4044_2_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("Predicted NRDA 4044 Cultch (Shell) Weight by Period - 1 quadrat")+
  #scale_x_continuous(breaks=seq(2,13,1))+
  ylim(0,10)
  
#########NRDA 5007 only#################

tmb_NRDA_5007 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNRDA_5007, family="nbinom2") #converge
summary(tmb_NRDA_5007)

dNRDA_5007.new = data.frame(Roundwt = dNRDA_5007$Roundwt,
                            Period = dNRDA_5007$Period,
                            Project = dNRDA_5007$Project,
                            Num_quads = dNRDA_5007$Num_quads)

dNRDA_5007.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(log(Num_quads)), data = dNRDA_5007.new, family="nbinom2") #converge
ggpredict(dNRDA_5007.new.tmb1)


#below is specifying X quad
dNRDA_5007_pred = ggpredict(dNRDA_5007.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
plot(dNRDA_5007_pred, facet=FALSE, add.data=TRUE)

#this predicts with 1 quad which is how we will compare the 3 projects
dNRDA_5007_2_pred = ggpredict(dNRDA_5007.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
plot(dNRDA_5007_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)

#note this matches the data well if you predict for 1-3 quads
#but to compare projects just predict for 1 quad.
#to keep checking if this is working correctly, just make this same
#subset for each study and make unique plots.

#jennifer way
pr4 = ggplot(dNRDA_5007_2_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Weight per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("Predicted Restore 5007 Cultch (Rock) Weight by Period - 1 quadrat") +
  #scale_x_continuous(breaks=seq(2,13,1))+
  ylim(0,10)


plot_grid(pr1,pr2,pr3,pr4)

ggsave("AB_predicted_weight.png", width = 10, height = 10)  


#next standardize x and y axis. Yeah!


#####END###############

#########################################

d3$Bottom[d3$Bay == "Pensacola" & d3$Project == "NRDA_4044" ] <- "Rock"
d3$Bottom[d3$Bay == "St. Andrews" & d3$Project == "NRDA_4044" ] <- "Rock"
d3$Bottom[d3$Bay == "Apalachicola" & d3$Project == "NRDA_4044" ] <- "Shell"
d3$Bottom[d3$Bay == "Apalachicola" & d3$Project == "NFWF_1" ] <- "Shell"
d3$Bottom[d3$Bay == "Apalachicola" & d3$Project == "FWC_2021" ] <- "Rock"
d3$Bottom[d3$Bay == "Apalachicola" & d3$Project == "NRDA_5007" ] <- "Rock"

########################################
##pause and just look at Apalachicola because you have shell and rock there only

d3.1<- subset(d3, d3$Bay == "Apalachicola")

tmb0.1 <- glmmTMB(Roundwt ~ Period + Bottom + (1|Site) + offset(log(Num_quads)), data = d3.1, family="nbinom2") #converge
summary(tmb0.1)

#this suggests you have a difference in bottom type.

ggpredict(tmb0.1)
pred_tmb0.1 = ggpredict(tmb0.1, terms = c("Period", "Bottom", "Num_quads[1]"), type = c('fe')) #for all projects
plot(pred_tmb0.1, facet=TRUE, colors=c("red","black"), add.data=FALSE)

#now use interaction term so different slopes, which is
#really what you want to know so you understand decay rate

tmb1.1 <- glmmTMB(Roundwt ~ Period * Bottom + (1|Site) + offset(log(Num_quads)), data = d3.1, family="nbinom2") #converge
summary(tmb1.1)

ggpredict(tmb1.1)
pred_tmb1.1 = ggpredict(tmb1.1, terms = c("Period", "Bottom", "Num_quads[1]"), type = c('fe')) #for all projects
plot(pred_tmb1.1, facet=TRUE, colors=c("red","black"), add.data=FALSE)
plot(pred_tmb1.1, facet=FALSE, add.data=TRUE)

AICtab(tmb0.1, tmb1.1)

################################
#this model is asking how period and bay influence weight

tmb1 <- glmmTMB(Roundwt ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
summary(tmb1)


#interaction term for different slopes
tmb2 <- glmmTMB(Roundwt ~ Period * Bottom + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
summary(tmb2)


###
library(ggeffects)

new.dat1 = data.frame(Roundwt = d3$Roundwt,
                      Period = d3$Period,
                      Project = d3$Project,
                      Num_quads = d3$Num_quads)

new.tmb1 <- glmmTMB(Roundwt ~ Period + Project + offset(log(Num_quads)), data = new.dat1, family="nbinom2") #converge
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

