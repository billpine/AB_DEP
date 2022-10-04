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
library(emmeans)
library(DHARMa)
library(readr)
library(jtools)

#this has all 3 bays
d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")

unique(d0$Bay)
#ok, change Apalachicola Bay to Apalachicola
d0.1<-d0 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))
d0.11<-d0.1 %>%
  mutate(Bay = replace(Bay,Bay == "St. Andrews", "St. Andrew"))

#updating the cultch amounts and removing the zero cultch
d0.2<-d0.11 %>%
  mutate(Cultch = replace(Cultch,Cultch == "999", "300"))
d0.3<- subset(d0.2, d0.2$Cultch > "0")

d1<-d0.3

dApalach<-subset(d0.3,d0.3$Bay =="Apalachicola")

#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Seed[d2$Seed < -1] <- NA
d2$Legal[d2$Legal < -1] <- NA



data_sum<- d2 %>%
  dplyr::group_by(Bay, Project, Year, Month, Period, Site) %>%
  dplyr::count(Bay, Project, Year,Month, Period, Site) %>%
  #dplyr::summarise(summarise(count = n()),na.rm=TRUE) %>%
  dplyr::relocate(Bay, Project, Year, Month, Period, Site)
names(data_sum) <- c("Bay","Project", "Year", "Month","Period", "Site",
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
  arrange(Period,Bay, Project, Year, Month, Site)

quad_sumx<- d2 %>%
  dplyr::group_by(Bay,Project, Period, Year, Month) %>%
  dplyr::count(Bay,Project, Period, Year, Month)

#just writing the table with number of quadrats by year, month, station to folder
#write.table(dx, file = "quad_count_bay_project_yr_mnth_station.txt", row.names = FALSE,
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
sum_wt <- dplyr::rename(wt,Bay=Bay, Project=Project, Site=Site,Period=Period, Season=Season,Weight=Weight)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Weight~Bay+Project+Site+Period+Season,data=d2,length)
count_quads_weight <- dplyr::rename(count_quads,Bay=Bay,Site=Site,Period=Period, Season=Season,Num_quads=Weight)

#merge weight total data frame with the tran_length total data frame
d3=merge(sum_wt,count_quads_weight,by=c("Bay", "Project","Site", "Period", "Season"))

#calculate CPUE. Just for fun to plot
d3$CPUE_Weight<-d3$Weight/d3$Num_quads

CPUE_FWC_2021<-subset(d3,d3$Project =="FWC_2021")
CPUE_NFWF_1<-subset(d3,d3$Site =="NFWF_1")
CPUE_NRDA_4044<-subset(d3,d3$Site =="NRDA_4044")
CPUE_NRDA_5007<-subset(d3,d3$Site =="NRDA_5007")

#this is by study on one plot 
r1<-ggplot(data = d3[d3$Project=="NFWF-1",], aes(Period, CPUE_Weight, color="NFWF_1")) +
  geom_point(size=3) +
  geom_point(data = d3[d3$Project=="NRDA-4044",], mapping = aes(Period, CPUE_Weight, color="NRDA_4044"), size = 3)+
  geom_point(data = d3[d3$Project=="GEBF-5007",], mapping = aes(Period, CPUE_Weight, color="NRDA_5077"), size = 3)+
  geom_point(data = d3[d3$Project=="FWC-2021",], mapping = aes(Period, CPUE_Weight, color="FWC_2021"), size = 3)+
  ggtitle("Weight per quadrat") +
  #scale_y_log10()+
  xlab("Period") +
  ylab("Weight")
#facet_wrap(~Project)

#this is by study on one plot 
r2<-ggplot(data = d3, aes(Period, CPUE_Weight, color=Project)) +
  geom_point(size=3)+
  facet_wrap(~Bay)

r2+scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(2,15),breaks=seq(2,15,2))+
  ylab("Weight (kg) per quadrat")

#ggsave("weight.png", width = 10, height = 10)  


######
#glmmTMB approach
######
######################

library(glmmTMB)
library(bbmle)

#round weight to make it integer
d3$Roundwt<-round(d3$Weight,0)

#just the rounded weights, not accounting for different # quads
r3<-ggplot(data = d3, aes(Period, Roundwt, color=Project)) +
  geom_point(size=3)+
  facet_wrap(~Bay)

#######
#######
#REVISE GLM to Ben way
#first analyze all bays
#then analyze just apalach


as.integer(d3$Roundwt)

## BMB: the same but 'looks' like an interaction
d3$SP <- with(d3, interaction(Site, Project, sep = "_", drop = TRUE))

#I'm using site bc the sites are uniquely named
tab <- with(d3,table(Site,Bay))
library(Matrix)
ifun <- function(M) {
  M <- as(M, "Matrix")
  image(M, aspect = "fill",
        scales = list(y = list(at = seq(nrow(M)), labels = rownames(M)),
                      x = list(at = seq(ncol(M)), labels = colnames(M), rot = 90)))
}
ifun(tab)

## sum-to-zero contrasts so main effect of Period = unweighted average across Bays
options(contrasts = c("contr.sum", "contr.poly"))

#this is Ben's plot of the data
ggplot(d3, aes(Period, Roundwt)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())

#Intercept
tmb0 <- glmmTMB(Roundwt ~ (1|Site) + offset(log(Num_quads)),
                data = d3, family="nbinom2") #converge
summary(tmb0)

#Period
tmb1 <- update(tmb0, . ~ . + Period)
summary(tmb1)

#Period + Bay
tmb2 <- update(tmb1, . ~ . + Bay)
summary(tmb2)

#Period*Bay
tmb3 <- update(tmb2, . ~ . + Period:Bay)
summary(tmb3)

#Bay
tmb4 <- update(tmb0, . ~ . + Bay)
summary(tmb4)

## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC


#bill note on tmb5. Ben removes the random effect term on site and then nests it under
#period

#Period*bay/site (allow period by site across bay)
tmb5 <- update(tmb3, . ~ . - (1|Site) + (Period|Site))

summary(tmb5)


tmb6 <- update(tmb5, dispformula = ~Bay)
summary(tmb6)

#drop tmb6 bc of convergence
cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4, tmb5)
modnames2 = c("intercept", "period", "period + bay", "period*bay", "bay", "period*bay/site")
names(cand.set2) <- modnames2



#model selection information

aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
aictab(cand.set2, modnames2, second.ord = TRUE) #model selection table with AICc


(em1 <- emtrends(tmb5, ~Bay, "Period"))
test(em1)

#ok so Apalach is significant, but is that b/c of shelling later periods
#need to look at Apalach only by project (just like the count analyses)

#########
#########
#NOW go just to Apalach
#########
#########

dApalach<-subset(d3,d3$Bay =="Apalachicola")


## BMB: the same but 'looks' like an interaction
dApalach$SP <- with(dApalach, interaction(Site, Project, sep = "_", drop = TRUE))


tab <- with(dApalach,table(SP,Project))
library(Matrix)
ifun <- function(M) {
  M <- as(M, "Matrix")
  image(M, aspect = "fill",
        scales = list(y = list(at = seq(nrow(M)), labels = rownames(M)),
                      x = list(at = seq(ncol(M)), labels = colnames(M), rot = 90)))
}
ifun(tab)

names(dApalach)

ggplot(dApalach, aes(Period, Roundwt)) + facet_wrap(~Project) + geom_point() + geom_line(aes(group = SP), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())

#Intercept
tmb0 <- glmmTMB(Roundwt ~ (1|SP) + offset(log(Num_quads)),
                   data = dApalach, family="nbinom2") #converge
summary(tmb0)


#Period
tmb1 <- update(tmb0, . ~ . + Period)
summary(tmb1)

#Period + Project
tmb2 <- update(tmb1, . ~ . + Project)
summary(tmb2)

#Period*Project
tmb3 <- update(tmb2, . ~ . + Period:Project)
summary(tmb3)

#Project
tmb4 <- update(tmb0, . ~ . + Project)
summary(tmb4)

#Ben note below
## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC

#bill note on tmb5. Ben removes the random effect term on site and then nests it
#period

#ben note below
#Period*Project/site (allow period by site across Project)
tmb5 <- update(tmb3, . ~ . - (1|SP) + (Period|SP))
diagnose(tmb5)  ##  BMB: this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5)

summary(tmb5)

#plot coefficients
plot_summs(tmb5)

#all + dispersion
tmb6 <- update(tmb5, dispformula = ~Project)
summary(tmb6)

#bp note, tmb6 is tmb5 + adding a unique dispersion parameter for each Project. 
#has singular convergence issue goes away with bfgs 

tmb7 <- update(tmb3, . ~ .  + (0+Period|SP), dispformula = ~Project)
summary(tmb7)

diagnose(tmb7)
VarCorr(tmb7)

#model selection information

## self-naming list
cand.set2 =
  list(tmb0, tmb1, tmb2, tmb3, tmb4, tmb5, tmb6, tmb7)
modnames2 = c("intercept", "period", "period + Project", "period*Project", "Project", "all",
                 "all + dispersion", "Project/sp uncorr + disp")
names(cand.set2) <- modnames2


#AIC
aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
#AICc
aictab(cand.set2, modnames2, second.ord = TRUE) #model selection table with AICc


(em1 <- emtrends(tmb7, ~Project, "Period"))
test(em1)



# ##############
# 
# ###below is all old old old
# 
# 
# 
# 
# ##subset by Project and fit simple models, no interaction terms needed this way
# unique(d3$Bay)
# 
# #Pensacola only
# dPensacola<-subset(d3,d3$Bay =="Pensacola")
# #apalach only
# dApalach<-subset(d3,d3$Bay =="Apalachicola")
# #St. Andrew only
# dStAndrew<-subset(d3,d3$Bay =="St. Andrew")
# 
# #now GLM models by Bay.  First Roundwt over Period
# 
# m1_Pensacola <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dPensacola, family="nbinom2") #converge
# summary(m1_Pensacola)
# #Pensacola declines over time. Makes sense b/c only received cultch once
# 
# m1_Apalach <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(m1_Apalach)
# #Increases over time. Makes since b/c cultched multiple times
# #Need to do Apalach by study to account for this
# 
# m1_StAndrew <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dStAndrew, family="nbinom2") #converge
# summary(m1_StAndrew)
# #declines over time.
# 
# 
# ##now dig into AB
# 
# 
# #All studies combined
# tmb0 <- glmmTMB(Roundwt ~ + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(tmb0)
# 
# #All studies combined over period
# tmb1 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(tmb1)
# 
# tmb2 <- glmmTMB(Roundwt ~ Project + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(tmb2)
# 
# tmb3 <- glmmTMB(Roundwt ~ Period + Project + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(tmb3)
# 
# tmb4 <- glmmTMB(Roundwt ~ Period * Project + (1|Site) + offset(log(Num_quads)), data = dApalach, family="nbinom2") #converge
# summary(tmb4)
# 
# 
# 
# 
# AICtab(tmb0,tmb1,tmb2,tmb3,tmb4,weights=TRUE)
# 
# 
# cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4)
# modnames2 = c("intercept", "period","project", "period + project", "period*project")
# aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
# 
# #####
# #now predict for best fit model (interaction term)
# library(ggeffects)
# 
# ggpredict(tmb4)
# pred_tmb3 <- ggpredict(tmb4, c("Period[15]", "Project[NFWF-1]","Num_quads[1]"))
# pred_tmb4 <- ggpredict(tmb4, c("Period[15]", "Project","Num_quads[1]"))
# plot(pred_tmb1, facet=TRUE, colors=c("red","black","blue","orange"), add.data=TRUE)
# 
# 
# 
# 
# ######
# ######
# ######
# 
# 
# #subset data for each project in Apalach, fit tmbglm, then predict, then separate plot
# #for each project. This is to check on intercepts
# 
# unique(dApalach$Project)
# 
# dNFWF_1<-subset(dApalach,dApalach$Project =="NFWF-1")
# dFWC_2021<-subset(dApalach,dApalach$Project =="FWC-2021")
# dNRDA_4044<-subset(dApalach,dApalach$Project =="NRDA-4044")
# dNRDA_5007<-subset(dApalach,dApalach$Project =="GEBF-5007")
# 
# #########NFWF_1 only#################
# #NFWF_1 this is working
# tmb_NFWF <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNFWF_1, family="nbinom2") #converge
# summary(tmb_NFWF)
# #looks like NFWF 1 declines over time
# 
# library(ggeffects)
# 
# dNFWF_1.new = data.frame(Roundwt = dNFWF_1$Roundwt,
#                          Period = dNFWF_1$Period,
#                          Project = dNFWF_1$Project,
#                          Num_quads = log(dNFWF_1$Num_quads))
# 
# dNFWF_1.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dNFWF_1.new, family="nbinom2") #converge
# ggpredict(dNFWF_1.new.tmb1)
# 
# #below is not specifying # quads so it just picks 3 values near 5
# dNFWF_1_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
# plot(dNFWF_1_pred, facet=FALSE, add.data=TRUE)
# 
# #the above is a good demonstration the model is a decent fit to data
# 
# #this predicts with 1 quad all periods which is how we will compare the 3 projects
# dNFWF_2_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(dNFWF_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)
# 
# #note this matches the data well if you predict for 5 quads
# #but to compare projects just predict for 1 quad.
# #to keep checking if this is working correctly, just make this same
# #subset for each study and make unique plots.
# 
# #jennifer way
# pr1 = ggplot(dNFWF_2_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("Predicted NFWF_1 Cultch (Shell) Weight by Period - 1 quadrat")+
#   #scale_x_continuous(breaks=seq(1,14,1))+
#   ylim(0,30)+
#   xlim(1,15)
# 
# 
# #no need to include the data on single quadrat because the data are from
# #more than 1 quadrat
# # +
# #   geom_point(data = dApalach[dApalach$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
# #   scale_x_continuous(breaks=seq(1,14,1))
# 
# #########FWC_2021 only#################
# #this one is tricky because it is only a few years
# 
# tmb_FWC_2021 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dFWC_2021, family="nbinom2") #converge
# summary(tmb_FWC_2021)
# 
# dFWC_2021.new = data.frame(Roundwt = dFWC_2021$Roundwt,
#                            Period = dFWC_2021$Period,
#                            Project = dFWC_2021$Project,
#                            Num_quads = log(dFWC_2021$Num_quads))
# 
# dFWC_2021.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dFWC_2021.new, family="nbinom2") #converge
# ggpredict(dFWC_2021.new.tmb1)
# 
# 
# #below is not specifying # quads so it just picks 3 values from 1.5 to 3.5
# dFWC_2021_pred = ggpredict(dFWC_2021.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
# plot(dFWC_2021_pred, facet=FALSE, add.data=TRUE)
# 
# 
# #this predicts with 1 quad which is how we will compare the 3 projects
# dFWC_2_pred = ggpredict(dFWC_2021.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(dFWC_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)
# 
# #note this matches the data well if you predict for 1-3 quads
# #but to compare projects just predict for 1 quad.
# #to keep checking if this is working correctly, just make this same
# #subset for each study and make unique plots.
# 
# #jennifer way
# pr2 = ggplot(dFWC_2_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("Predicted FWC_2021 Cultch (Rock) Weight by Period - 1 quadrat") +
#   ylim(0,40)+
#   xlim(1,15)
# 
# #no need to include the data on single quadrat because the data are from
# #more than 1 quadrat
# # +
# #   geom_point(data = dApalach[dApalach$Project == "FWC_2021",], mapping = aes(Period, Roundwt), size = 2)+
# #   scale_x_continuous(breaks=seq(1,14,1))
# 
# #########NRDA 4044 only#################
# 
# tmb_NRDA_4044 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNRDA_4044, family="nbinom2") #converge
# summary(tmb_NRDA_4044)
# 
# dNRDA_4044.new = data.frame(Roundwt = dNRDA_4044$Roundwt,
#                             Period = dNRDA_4044$Period,
#                             Project = dNRDA_4044$Project,
#                             Num_quads = log(dNRDA_4044$Num_quads))
# 
# dNRDA_4044.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dNRDA_4044.new, family="nbinom2") #converge
# ggpredict(dNRDA_4044.new.tmb1)
# 
# 
# #below is not specifying # quads so it just picks 3 values from 2 to 4.5
# dNRDA_4044_pred = ggpredict(dNRDA_4044.new.tmb1, terms = c("Period", "Num_quads"), type = c('fe')) #for all projects
# plot(dNRDA_4044_pred, facet=FALSE, add.data=TRUE)
# 
# #the above is a good demonstration the model is a decent fit to data
# 
# #this predicts with 1 quad which is how we will compare the 3 projects
# dNRDA_4044_2_pred = ggpredict(dNRDA_4044.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(dNRDA_4044_2_pred, facet=FALSE, colors=c("red"), add.data=FALSE)
# 
# #note this matches the data well if you predict for 1-3 quads
# #but to compare projects just predict for 1 quad.
# #to keep checking if this is working correctly, just make this same
# #subset for each study and make unique plots.
# 
# #jennifer way
# pr3 = ggplot(dNRDA_4044_2_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("Predicted NRDA 4044 Cultch (Shell) Weight by Period - 1 quadrat")+
#   ylim(0,30)+
#   xlim(1,15)
# 
# #########NRDA 5007 only#################
# 
# tmb_NRDA_5007 <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNRDA_5007, family="nbinom2") #converge
# summary(tmb_NRDA_5007)
# 
# dNRDA_5007.new = data.frame(Roundwt = dNRDA_5007$Roundwt,
#                             Period = dNRDA_5007$Period,
#                             Project = dNRDA_5007$Project,
#                             Num_quads = log(dNRDA_5007$Num_quads))
# 
# dNRDA_5007.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dNRDA_5007.new, family="nbinom2") #converge
# ggpredict(dNRDA_5007.new.tmb1)
# 
# 
# #below is not specifying # quads so it just picks 3 values from 2 to 4.5
# dNRDA_5007_pred = ggpredict(dNRDA_5007.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(dNRDA_5007_pred, facet=FALSE, add.data=FALSE)
# 
# #the above is a good demonstration the model is a decent fit to data
# 
# #this predicts with 1 quad which is how we will compare the 3 projects
# dNRDA_5007_2_pred = ggpredict(dNRDA_5007.new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(dNRDA_5007_2_pred, facet=FALSE, colors=c("red"), add.data=TRUE)
# 
# ##note this matches the data well if you predict for 1-3 quads
# #but to compare projects just predict for 1 quad.
# #to keep checking if this is working correctly, just make this same
# #subset for each study and make unique plots.
# 
# #jennifer way
# pr4 = ggplot(dNRDA_5007_2_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("Predicted Restore 5007 Cultch (Rock) Weight by Period - 1 quadrat") +
#   #scale_x_continuous(breaks=seq(2,14,1))+
#   ylim(0,30)+
#   xlim(1,15)
#   
# 
# plot_grid(pr1,pr2,pr3,pr4)
# 
# ggsave("predicted_weight.pdf", width = 10, height = 10)  
# 
# 
# #next standardize x and y axis. Yeah!
# 
# ####all below on the bench
# 
# 
# #########################################
# #this model is asking how period and project influence weight
# #using NB2 formulation (most common)
# tmb1 <- glmmTMB(Roundwt ~ Period + Project + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
# summary(tmb1)
# 
# 
# #interaction term for different slopes
# tmb2 <- glmmTMB(Roundwt ~ Period * Project + (1|Site) + offset(log(Num_quads)), data = d3, family="nbinom2") #converge
# summary(tmb2)
# 
# 
# ###
# library(ggeffects)
# 
# new.dat1 = data.frame(Roundwt = d3$Roundwt,
#                       Period = d3$Period,
#                       Project = d3$Project,
#                       Num_quads = log(d3$Num_quads))
# 
# new.tmb1 <- glmmTMB(Roundwt ~ Period + Project + offset(Num_quads), data = new.dat1, family="nbinom2") #converge
# ggpredict(new.tmb1)
# pred_tmb1 = ggpredict(new.tmb1, terms = c("Period", "Project", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(pred_tmb1, facet=TRUE, colors=c("red","black","blue","orange"), add.data=FALSE)
# plot(pred_tmb1, facet=FALSE, colors=c("red","black","blue","orange"), add.data=FALSE)
# 
# ####
# new.tmb2 <- glmmTMB(Roundwt ~ Period * Project + offset(Num_quads), data = new.dat1, family="nbinom2") #converge
# ggpredict(new.tmb2)
# pred_tmb2 = ggpredict(new.tmb2, terms = c("Period", "Project", "Num_quads[1]"), type = c('fe')) #for all projects
# plot(pred_tmb2, facet=TRUE, colors=c("red","black","blue","orange"), add.data=FALSE)
# 
# #if you run pred_tmb2 without the [1] it calculates for average
# #number of quads which is like 4.27. So then I predicted in pred_tmb3
# #for 5 quads and the overlay w/ data looks good for NFWF 1 but not
# #other projects
# 
# pred_tmb3 = ggpredict(new.tmb2, terms = c("Period", "Project", "Num_quads[5]"), type = c('fe')) #for all projects
# plot(pred_tmb3, facet=TRUE, colors=c("red","black","blue","orange"), add.data=TRUE)
# 
# 
# #this is plotting for 1 quadrat. If you add the real data, that is for a lot of
# #quadrats so that's why the predicted is WAY less than real
# 
# 
# ####
# 
# 
# nfwf_pred<- subset(pred_tmb1, pred_tmb1$group == "NFWF_1")
# pr1 = ggplot(nfwf_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("NFWF_1 Weight by Period") +
#   geom_point(data = d3[d3$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))
# 
# NRDA_4044_pred<- subset(pred_tmb1, pred_tmb1$group == "NRDA_4044")
# pr2 = ggplot(NRDA_4044_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("NRDA_4044 Weight by Period") +
#   geom_point(data = d3[d3$Project == "NRDA_4044",], mapping = aes(Period, Roundwt), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))
# 
# 
# #######
# 
# #subset data for each project, fit tmbglm, then predict, then separate plot
# #for each project
# 
# unique(d3$Project)
# 
# dNFWF_1<-subset(d3,d3$Project =="NFWF_1")
# dFWC_2021<-subset(d3,d3$Project =="FWC_2021")
# dNRDA_4044<-subset(d3,d3$Project =="NRDA_4044")
# dNRDA_5007<-subset(d3,d3$Project =="NRDA_5007")
# #########NFWF_1 only#################
# #NFWF_1 this is working
# tmb_NFWF <- glmmTMB(Roundwt ~ Period + (1|Site) + offset(log(Num_quads)), data = dNFWF_1, family="nbinom2") #converge
# summary(tmb_NFWF)
# 
# 
# dNFWF_1.new = data.frame(Roundwt = dNFWF_1$Roundwt,
#                       Period = dNFWF_1$Period,
#                       Project = dNFWF_1$Project,
#                       Num_quads = log(dNFWF_1$Num_quads))
# 
# dNFWF_1.new.tmb1 <- glmmTMB(Roundwt ~ Period + offset(Num_quads), data = dNFWF_1.new, family="nbinom2") #converge
# ggpredict(dNFWF_1.new.tmb1)
# dNFWF_1_pred = ggpredict(dNFWF_1.new.tmb1, terms = c("Period", "Num_quads[5]"), type = c('fe')) #for all projects
# plot(dNFWF_1_pred, facet=FALSE, colors=c("red"), add.data=TRUE)
# 
# 
# #note this matches the data well if you predict for 5 quads
# #but to compare projects just predict for 1 quad.
# #to keep checking if this is working correctly, just make this same
# #subset for each study and make unique plots.
# #need to get jennifer way to work.  
# 
# #jennifer way
# nfwf_pred<- subset(pred_tmb1, pred_tmb1$group == "NFWF_1")
# pr1 = ggplot(nfwf_pred, aes(x, predicted))+
#   geom_line(size=2)+
#   ylab("Weight per quad") +
#   xlab ("Period")+
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
#   ggtitle("NFWF_1 Weight by Period") +
#   geom_point(data = d3[d3$Project == "NFWF_1",], mapping = aes(Period, Roundwt), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))
# 
