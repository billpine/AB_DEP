
#this is the DEP data from project 4044
#but for all three bays (pensacola, east bay, apalach)

d1 <- read.csv("~/GitHub/AB_DEP/20220326_merged_agency_data.csv")

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

library(tidyr)

names(d1)
head(d1)

#now remove the 3 or 4 NA's from Pensacola (lost samples?)
d2 <- filter(d1, Legal != "NA")

names(d2)
#check bays
unique(d2$Bay)

#ok, change Apalachicola Bay to Apalachicola
d2<-d2 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))

#check sites
unique(d2$Site)

#just check to see if other NAs
which(is.na(d2), arr.ind=TRUE)
#this is one NA for weight, but it has counts
max(d2$Legal)
max(d2$Seed)
max(d2$Spat)
######

#look at each bay or similar
dx<-subset(d2,d2$Bay == "Apalachicola")
#you see it is a mix of cultch and studies

#count number of quadrats per period, site
quad_sum<- d2 %>%
  dplyr::group_by(Period, Bay) %>%
  dplyr::count(Period, Bay) %>%
  dplyr::relocate(Period, Bay)
names(quad_sum) <- c("Period", "Bay",
                     "Number_Quadrats")
 
# ##summary number of oyster spat counted each period
 spat_sum <- d2 %>%
   dplyr::group_by(Bay, Period) %>%
   dplyr::summarise(sum=sum(Spat,na.rm=TRUE)) %>%
   dplyr::arrange(Bay, Period)
# 
 names(spat_sum) <- c("Bay","Period",
                      "Number Live Spat")

###########################
###########################
#below I sum oysters in each size class by bay and period
#This combines all of the sites together for a bay and period
#this increases the number of oysters because the sites
#are summed, but it reduces the information from each site
#within a bay
###########################
###########################


#sum live counts for each transect
count_spat=aggregate(Spat~Bay+Period,data=d2,sum)
count_spat <- dplyr::rename(count_spat,Bay=Bay,Period=Period,Sum_spat=Spat)

count_seed=aggregate(Seed~Bay+Period,data=d2,sum)
count_seed <- dplyr::rename(count_seed,Bay=Bay,Period=Period,Sum_seed=Seed)

count_legal=aggregate(Legal~Bay+Period,data=d2,sum)
count_legal <- dplyr::rename(count_legal,Bay=Bay,Period=Period,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Spat~Bay+Period,data=d2,length)
count_quads_spat <- dplyr::rename(count_quads,Bay=Bay,Period=Period,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d5=merge(count_spat,count_quads_spat,by=c("Bay", "Period"))
d5.1=merge(d5,count_seed,by=c("Bay", "Period"))
d5.2=merge(d5.1,count_legal,by=c("Bay", "Period"))

d6<-d5.2

#calculate CPUE. Just for fun to plot
d6$CPUE_Spat<-d6$Sum_spat/d6$Num_quads
d6$CPUE_Seed<-d6$Sum_seed/d6$Num_quads
d6$CPUE_Legal<-d6$Sum_legal/d6$Num_quads

f1<-ggplot(d6, aes(Period, CPUE_Spat)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

f2<-ggplot(d6, aes(Period, CPUE_Seed)) +
  geom_point(size=4) +
  ggtitle("Seed CPUE by Period") +
  xlab("Period") +
  ylab("Seed") +
  facet_wrap(~Bay)

f3<-ggplot(d6, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(f1,f2,f3)

#ggsave("allbays_cpue.pdf", width = 10, height = 10)

#############
#The CPUE was just for visual
#############


#####################################
#Now I work on some basic nbGLMs using the counts
#I'm not working with CPUE
#####################################

#plots of total counts and then GLMs
f4<-ggplot(d6, aes(Period, Sum_spat)) +
  geom_point(size=4) +
  ggtitle("Spat Sum by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

f5<-ggplot(d6, aes(Period, Sum_seed)) +
  geom_point(size=4) +
  ggtitle("Seed Sum by Period") +
  xlab("Period") +
  ylab("Seed") +
  facet_wrap(~Bay)

f6<-ggplot(d6, aes(Period, Sum_legal)) +
  geom_point(size=4) +
  ggtitle("Legal Sum by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(f4,f5,f6)

mean(d6$Sum_spat)
var(d6$Sum_spat)

mean(d6$Sum_sublegal)
var(d6$Sum_sublegal)

mean(d6$Sum_legal)
var(d6$Sum_legal)

#variances are greater than means for each size class
#data from all bays combined

#suggests NB

names(d6)

qqnorm(d6$Sum_spat)
qqnorm(d6$Sum_sublegal)
qqnorm(d6$Sum_legal)

#qqnorms look overdispersed

library(vcd)

plot(d6$Sum_spat~d6$Period)

points(d6$Sum_spat[d6$Bay=="Pensacola"] ~ 
         d6$Period[d6$Bay=="Pensacola"], col='green', pch=16)
points(d6$Sum_spat[d6$Bay=="St. Andrews"] ~ 
         d6$Period[d6$Bay=="St. Andrews"], col='blue', pch=15)
points(d6$Sum_spat[d6$Bay=="Apalachicola"] ~ 
         d6$Period[d6$Bay=="Apalachicola"], col='black', pch=17)
#legend(x='topleft', fill=c('green','blue','black'), 
#       legend=c('Pensacola','St. Andrews', 'Apalach'))

spat.lambda<-fitdistr(d6$Sum_spat,"normal")
gf.spat<-goodfit(d6$Sum_spat, type= "nb", method="ML")

d6$Bay <- as.factor(d6$Bay)

#################################################
#fit basic NB GLM
#below is not with site as random effect so I turned it off
################################################# 
# library(glmmTMB)
# library(bbmle)
# 
# m1 <- glm.nb(Sum_spat ~ Bay + offset(log(Num_quads)), data = d6) 
# m2 <- glm.nb(Sum_spat ~ Bay + Period + offset(log(Num_quads)), data = d6) 
# m3 <- glm.nb(Sum_spat ~ Bay * Period + offset(log(Num_quads)),data = d6, control = glm.control(maxit = 5000)
# ) 
# 
# cand.set = list(m1,m2,m3)
# modnames = c("bay", "bay+period", "bay*period")
# AICtab(m1,m2,m3) #model selection table with AIC
# 
# summary(m1)
# summary(m2)
# summary(m3)
# 
# #results suggests for spat bays are different
# 
# m1.1 <- glm.nb(Sum_sublegal ~ Bay + offset(log(Num_quads)), data = d6) 
# m2.1 <- glm.nb(Sum_sublegal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
# m3.1 <- glm.nb(Sum_sublegal ~ Bay * Period + offset(log(Num_quads)), data = d6) 
# 
# cand.set2 = list(m1.1,m2.1,m3.1)
# modnames2 = c("bay", "bay+period", "bay*period")
# aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
# 
# summary(m2.1)
# #again, 
# 
# #legals
# m1.2 <- glm.nb(Sum_legal ~ Bay + offset(log(Num_quads)), data = d6) 
# m2.2 <- glm.nb(Sum_legal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
# m3.2 <- glm.nb(Sum_legal ~ Bay * Period + offset(log(Num_quads)), data = d6) 
# 
# cand.set3 = list(m1.2,m2.2,m3.2)
# modnames3 = c("bay", "bay+period", "bay*period")
# aictab(cand.set3, modnames3, second.ord = FALSE) #model selection table with AIC
# 
# summary(m2.2)
# #for legals, st. andrews differs from others but that's it

##################################################
##################################################
###now let's go back and just sum by bay and period and site
##then try random effects
##We will treat a site as the random effect, so all the quadrats
##at a site (such as redfish point) are all assumed to be "related"
##this differs from the earlier analyses where we summed all the sites
##################################################
##################################################


#sum live counts by bay, period, site 
count_spat_site=aggregate(Spat~Bay+Period+Site,data=d2,sum)
count_spat_site <- dplyr::rename(count_spat_site,Bay=Bay,Period=Period,Sum_spat=Spat, Site=Site)

count_seed_site=aggregate(Seed~Bay+Period+Site,data=d2,sum)
count_seed_site <- dplyr::rename(count_seed_site,Bay=Bay,Period=Period,Sum_seed=Seed,Site=Site)

count_legal_site=aggregate(Legal~Bay+Period+Site,data=d2,sum)
count_legal_site <- dplyr::rename(count_legal_site,Bay=Bay,Period=Period,Sum_legal=Legal, Site=Site)

#count number quads by doing the length of transect, then rename
count_quads_site=aggregate(Spat~Bay+Period+Site,data=d2,length)
count_quads_site <- dplyr::rename(count_quads_site,Bay=Bay,Period=Period,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d3=merge(count_spat_site,count_quads_site,by=c("Bay", "Period", "Site"))
d3.1=merge(d3,count_seed_site,by=c("Bay", "Period", "Site"))
d3.2=merge(d3.1,count_legal_site,by=c("Bay", "Period", "Site"))

d4<-d3.2


names(d4)

d5<-dplyr::select(d4,Bay, Period, Site, Num_quads, 
               Sum_spat,Sum_seed,Sum_legal)

#calculate CPUE. Just for fun to plot
d5$CPUE_Spat<-d5$Sum_spat/d5$Num_quads
d5$CPUE_seed<-d5$Sum_seed/d5$Num_quads
d5$CPUE_Legal<-d5$Sum_legal/d5$Num_quads

#the plots below show you the range in CPUE for a given size class
#of oyster in each of the bays

s1<-ggplot(d5, aes(Period, CPUE_Spat)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

s2<-ggplot(d5, aes(Period, CPUE_seed)) +
  geom_point(size=4) +
  ggtitle("seed CPUE by Period") +
  xlab("Period") +
  ylab("seed") +
  facet_wrap(~Bay)

s3<-ggplot(d5, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(s1,s2,s3)

#ggsave("dep_allbays_site.pdf", width = 10, height = 10)

#single plot of CPUE, spat, different color by Bay
s4<-ggplot(d5, aes(Period, CPUE_Spat, color=Bay)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat CPUE")

#########
##ok move away from CPUE and work on the GLMs
#########

qqnorm(d5$Sum_spat)
qqnorm(d5$Sum_seed)
qqnorm(d5$Sum_legal)

#yes overdispersed

#some GLMs

#single plot, sum spat
#this is the raw data, then you will fit nb.glm to these data 
#and use sampling site
#as random effect while controling for effort

r0<-ggplot(d5, aes(Period, Sum_spat)) +
  geom_point(size=4) +
  ggtitle("Spat per Period by Site") +
  xlab("Period") +
  ylab("Total Spat")+
  facet_wrap(~Bay)

r1<-ggplot(d9, aes(Period, Sum_seed)) +
  geom_point(size=4) +
  ggtitle("seed per Period by Site") +
  xlab("Period") +
  ylab("Total seed")+
  facet_wrap(~Bay)

r2<-ggplot(d9, aes(Period, Sum_legal)) +
  geom_point(size=4) +
  ggtitle("Legal per Period by Site") +
  xlab("Period") +
  ylab("Total Legal")+
  facet_wrap(~Bay)


# 
# pm <- plot_grid(
#   pm1 + theme(legend.position="none"),
#   pm2 + theme(legend.position="none"),
#   pm3 + theme(legend.position="none"),
#   align = 'vh',
#   hjust = -1,
#   nrow = 1,
#   ncol=3
# )


#for plotting could scale y axis better but that would 
#cause 1 point for St. Andrews to be lost

###############
##TMB##########
###############

library(glmmTMB)
library(bbmle)

names(d5)

tmb0 <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = d5, family="nbinom2") #converge
summary(tmb0)

##below is Period + Bay with site as random effect and effort offset
tmb1 <- glmmTMB(Sum_spat ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d5, family="nbinom2") #converge
summary(tmb1)

# #NB2 formulation
# tmb2 <- glmmTMB(Sum_spat ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d5, family="nbinom1") #converge
# summary(tmb2)
# 
# #zero inflated poisson
# tmb3<- glmmTMB(Sum_spat ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d9, ziformula=~1, family="poisson") #converge
# summary(tmb3)

#Period*Bay
tmb4 <- glmmTMB(Sum_spat ~ Period*Bay + (1|Site) + offset(log(Num_quads)), data = d5, family="nbinom2") #converge
summary(tmb4)

#which is best "family"
#turned off now as Nbinom2 was best
#AICtab(tmb1,tmb2,tmb3)

#tmb1 better fit of base models, tmb1 and tmb4 no difference
#really should just compare the tmb1,2,3 here to determine which family
#then using a common family, compare different models

#now different models
AICtab(tmb0,tmb1,tmb4)

#lower AIC with interaction term

##comparison only, just subset data and fit model w/ Period by Bay
#now subset and just work with each Bay individually
dPensacola<-subset(d5,d5$Bay =="Pensacola")
tmb1_Pensacola <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = dPensacola, family="nbinom2") #converge
summary(tmb1_Pensacola)

dStAndrews<-subset(d5,d5$Bay =="St. Andrews")
tmb1_StAndrews <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = dStAndrews, family="nbinom2") #converge
summary(tmb1_StAndrews)

dApalachicola<-subset(d5,d5$Bay =="Apalachicola")
tmb1_Apalachicola <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = dApalachicola, family="nbinom2") #converge
summary(tmb1_Apalachicola)
###########

#need to check this, but I think below is just plotting with a common slope
#b/c no interaction term

##key plot
library(ggeffects)

ggpredict(tmb1)

pred_tmb1 <- ggpredict(tmb1, c("Period", "Bay"))

plot(pred_tmb1, facet=TRUE, colors=c("red","black","blue"), add.data=TRUE)
#neat that works

#so think about whether separate slopes is needed (go over w/ Jennifer)
#to help interpret mgmt actions
#I think the interaction term is key
  
ggpredict(tmb4)

pred_tmb4 <- ggpredict(tmb4, c("Period", "Bay"))

plot(pred_tmb4, facet=TRUE, colors=c("red","black","blue"), add.data=TRUE)

#neat that works but different results than pred_tmb1 b/c
#of differences in slopes
#note that is predicting for 45.11 quadrats (the mean)
#here it is for 1 quadrat (not on log scale)


##predict for specific period and one quadrat (becuse this is log the [] needs 0 not 1 to be 1)
#that's because exp(0) is 1

#new.dat = data.frame(Sum_spat = d4$Sum_spat,
#                     Period = d4$Period,
#                     Num_quads = log(d4$Num_quads))

#new.tmb1 <- glmmTMB(Sum_spat ~ Period + offset(Num_quads), data = new.dat, family="nbinom2") #converge


#this way does not log transform number of quads, just works with number
#from the data
#but if you use this one you have to put log in the model
new.dat = data.frame(Sum_spat = d4$Sum_spat,
                     Period = d4$Period,
                     Num_quads = d4$Num_quads)

new.tmb1 <- glmmTMB(Sum_spat ~ Period + offset(log(Num_quads)), data = new.dat, family="nbinom2") #converge

ggpredict(new.tmb1)
test = ggpredict(new.tmb1, terms = c("Period", "Num_quads[1]"), type = c('fe'))

#across period for 1 quad what is predicted value
#plot(test, facet=TRUE, add.data=TRUE)


##now include Bay as interaction term and use new data frame

#note I moved the log num quads to the model from the new.dat2

new.dat2 = data.frame(Sum_spat = d4$Sum_spat,
                      Period = d4$Period,
                      Bay = d4$Bay,
                      Num_quads = d4$Num_quads)

new.tmb2 <- glmmTMB(Sum_spat ~ Period * Bay + offset(log(Num_quads)), data = new.dat2, family="nbinom2") #converge

#if you go back to the new.dat2 and change Num_quads to Num_quads = d4$Num_quads
#new.tmb2 <- glmmTMB(Sum_spat ~ Period * Bay + offset(log(Num_quads)), data = new.dat2, family="nbinom2") #converge

##this is key plot
#below is for all bays but 1 quad
ggpredict(new.tmb2)
test2 = ggpredict(new.tmb2, terms = c("Period[14]", "Bay", "Num_quads[1]"), type = c('fe')) #for all Bays

#across periods for 1 quad what is predicted value in each bay
plot(test2, facet=TRUE, add.data=FALSE,colors=c("red","black","blue") )
#neat that works

#the above works ok for mean number of quads and turning add.data = TRUE

## 
## ##Jennifer style
#using a model predicting for each Bay individually
#for each period and 1 quadrat
Pensacola<- subset(test2, test2$group == "Pensacola")
StAndrews<- subset(test2, test2$group == "St. Andrews")
Apalach<- subset(test2, test2$group == "Apalachicola")
# # 
# # 
pr1 = ggplot(Pensacola, aes(x, predicted))+
   geom_line(size=2)+
   ylab("Count per quad") +
   xlab ("Period")+
   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
   ggtitle("DEP Pensacola Live Spat by Period") 

#below is how you would add the data, but because we are predicting from 1 quadrat the data don't line uo
#well because in the field more than 1 quadrat was collected
#but each study has a really different number of quadrats, this is all
#for comparison purposes so predicting for 1 quadrat is ok I think

#+
#   geom_point(data = dp3.2[dp3.2$Project == "NFWF_1",], mapping = aes(Period, Sum_spat), size = 2)+
#   scale_x_continuous(breaks=seq(1,14,1))

pr2 = ggplot(Apalach, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Count per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("Apalachicola Live Spat by Period") 

pr3 = ggplot(StAndrews, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Count per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("St. Andrews Live Spat by Period")+
  ylim(0,10000)

plot_grid(pr1, pr2, pr3)

##look more at this
#this is a good plot

pr <- plot_grid(
   pr1 + theme(legend.position="none"),
   pr2 + theme(legend.position="none"),   
   pr3 + theme(legend.position="none"),
   align = 'vh',
   hjust = -1,
   nrow = 1,
   ncol=3
 )

##below needs more work

#####################################################
##seeds from best model above
###below is best model TMB1

##if you use this below, need to move the log term out of the data generator
#and put it in the model line as above


tmb1_seed <- glmmTMB(Sum_seed ~ Period * Bay + (1|Site) + offset(log(Num_quads)), data = d4, family="nbinom2") #converge
summary(tmb1_seed)

new.dat_sub = data.frame(Sum_seed = d4$Sum_seed,
                      Period = d4$Period,
                      Bay = d4$Bay,
                      Num_quads = d4$Num_quads)

new.tmb2_sub <- glmmTMB(Sum_seed ~ Period * Bay + offset(log(Num_quads)), data = new.dat_sub, family="nbinom2") #converge

##this is key plot
#below is for all bays but 1 quad
ggpredict(new.tmb2_sub)
test2_sub = ggpredict(new.tmb2_sub, terms = c("Period", "Bay", "Num_quads[1]"), type = c('fe')) #for all Bays
plot(test2_sub, facet=TRUE, add.data=TRUE,colors=c("red","black","blue") )
######################################################################


#####################################################
##LEGALS from best model above
###below is best model TMB1
tmb1_legal <- glmmTMB(Sum_legal ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d4, family="nbinom2") #converge
summary(tmb1_legal)

new.dat_legal = data.frame(Sum_legal = d4$Sum_legal,
                         Period = d4$Period,
                         Bay = d4$Bay,
                         Num_quads = d4$Num_quads)

new.tmb2_legal <- glmmTMB(Sum_legal ~ Period + Bay + offset(log(Num_quads)), data = new.dat_legal, family="nbinom2") #converge

##this is key plot
#below is for all bays but 1 quad
ggpredict(new.tmb2_legal)
test2_legal = ggpredict(new.tmb2_legal, terms = c("Period[14]", "Bay", "Num_quads[1]"), type = c('fe')) #for all Bays
plot(test2_legal, facet=TRUE, add.data=TRUE,colors=c("red","black","blue") )
######################################################################




# 
# #plot ideas
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# #https://mran.microsoft.com/snapshot/2017-04-22/web/packages/sjPlot/vignettes/sjpglm.html
# https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/plotting-functions-ggplotsci.html#estimate-response-and-effects-with-emmeans


5.3825+-3.32295
