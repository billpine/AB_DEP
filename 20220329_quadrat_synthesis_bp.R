#Apalachicola oyster data from two DEP files and 1 FWC file 

###REMEMBER I SUBSET BELOW TO JUST WORK WITH APALACHICOLA

#data from quadrats
#Bill Pine
#January and February 2022

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


d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")

#ok, change Apalachicola Bay to Apalachicola
d0.1<-d0 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))

d1<- subset(d0.1, d0.1$Bay == "Apalachicola")


#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Seed[d2$Seed < -1] <- NA
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

##summary number of oyster spat counted each month
#by Project, Year, Month, Period, Site
spat_sum_z<-spat_sum <- d2 %>%
  dplyr::group_by(Project, Year, Month, Period, Site) %>%
  dplyr::summarise(sum=sum(Spat,na.rm=TRUE)) %>%
  dplyr::arrange(Project, Year, Month, Period, Site)

spat_sum_zz<-dplyr::arrange(spat_sum_z,Period)


names(spat_sum_zz) <- c("Project", "Year", "Month","Period", "Site",
                     "Number Live Spat")

#  write.table(spat_sum_zz, file = "spat_count_yr_mnth_station.txt", row.names = FALSE,
#              col.names = TRUE,sep = ",")
  
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

count_Seed=aggregate(Seed~Site+Period+Season,data=d2,sum)
count_Seed <- dplyr::rename(count_Seed,Site=Site,Period=Period, Season=Season,Sum_Seed=Seed)

count_legal=aggregate(Legal~Site+Period+Season,data=d2,sum)
count_legal <- dplyr::rename(count_legal,Site=Site,Period=Period, Season=Season,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Spat~Site+Period+Season,data=d2,length)
count_quads_spat <- dplyr::rename(count_quads,Site=Site,Period=Period, Season=Season,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d3=merge(count_spat,count_quads_spat,by=c("Site", "Period", "Season"))
d3.1=merge(d3,count_Seed,by=c("Site", "Period", "Season"))
d3.2=merge(d3.1,count_legal,by=c("Site", "Period", "Season"))

d3<-d3.2

#calculate CPUE. Just for fun to plot
d3$CPUE_Spat<-d3$Sum_spat/d3$Num_quads
d3$CPUE_Seed<-d3$Sum_Seed/d3$Num_quads
d3$CPUE_Legal<-d3$Sum_legal/d3$Num_quads


plot(d3$Period,d3$CPUE_Spat)
plot(d3$Period,d3$CPUE_Seed)
plot(d3$Period,d3$CPUE_Legal)

CPUE_Cat<-subset(d3,d3$Site =="Cat Point")
CPUE_Hotel<-subset(d3,d3$Site =="Hotel Bar")
CPUE_Dry<-subset(d3,d3$Site =="Dry Bar")
CPUE_Bulkhead<-subset(d3,d3$Site =="Bulkhead")

windows(record=TRUE)


f1<-ggplot(CPUE_Cat, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Cat Point Legal") +
  xlim(0,14)+
  xlab("Period") +
  ylab("CPUE Legal")

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


plot_grid(f1,f2,f3,f4)


##maybe start here with FWC/FSU
##this will show CPUE of spat, seed, legal by site over period

f5<-ggplot(d3, aes(Period, CPUE_Spat)) +
  geom_point(size=2) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  scale_x_continuous(breaks=seq(2,13,1))+
  facet_wrap(~Site)
#ggsave("AB_spat_period_site.pdf", width = 10, height = 10)

#now seed

f5.1<-ggplot(d3, aes(Period, CPUE_Seed)) +
  geom_point(size=2) +
  ggtitle("Seed CPUE by Period") +
  xlab("Period") +
  ylab("Seed") +
  scale_x_continuous(breaks=seq(2,13,1))+
  facet_wrap(~Site)
#ggsave("Seed.pdf", width = 10, height = 10)

#Berrigan 1990 in Table 2 had about 98 per 1/4 m2 quadrat > 25-mm 18 months after restoration
#and harvest season

#now legal

f5.2<-ggplot(d3, aes(Period, CPUE_Legal)) +
  geom_point(size=2) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  scale_x_continuous(breaks=seq(2,13,1))+
  facet_wrap(~Site)
#ggsave("legal.pdf", width = 10, height = 10)

#Berrigan 1990 in Table 2 had about 98 per 1/4 m2 quadrat > 25-mm 18 months after restoration
#and harvest season

####now let's go back and see if this matters by study

#sum live counts for each transect
count_spat2=aggregate(Spat~Project+Period+Site,data=d2,sum)
count_spat2 <- dplyr::rename(count_spat2,Project=Project,Period=Period,Site=Site,Sum_spat=Spat)

count_Seed2=aggregate(Seed~Project+Period+Site,data=d2,sum)
count_Seed2 <- dplyr::rename(count_Seed2,Project=Project,Period=Period,Site=Site,Sum_Seed=Seed)

count_legal2=aggregate(Legal~Project+Period+Site,data=d2,sum)
count_legal2 <- dplyr::rename(count_legal2,Project=Project,Period=Period,Site=Site,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads2=aggregate(Spat~Project+Period+Site,data=d2,length)
count_quads_spat2 <- dplyr::rename(count_quads2,Project=Project,Period=Period,Site=Site,Num_quads=Spat)

count_quadsxx=aggregate(Spat~Period,data=d2,length)
count_quads_spatxx <- dplyr::rename(count_quadsxx,Period=Period,Num_quads=Spat)


#merge spat live count total data frame with the tran_length total data frame
dp3=merge(count_spat2,count_quads_spat2,by=c("Project","Period","Site"))
dp3.1=merge(dp3,count_Seed2,by=c("Project", "Period","Site"))
dp3.2=merge(dp3.1,count_legal2,by=c("Project", "Period","Site"))

names(dp3.2)


#calculate CPUE. Just for fun to plot
dp3.2$CPUE_Spat<-dp3.2$Sum_spat/dp3.2$Num_quads
dp3.2$CPUE_Seed<-dp3.2$Sum_Seed/dp3.2$Num_quads
dp3.2$CPUE_Legal<-dp3.2$Sum_legal/dp3.2$Num_quads


plot(dp3.2$Period,dp3.2$CPUE_Spat)
plot(dp3.2$Period,dp3.2$CPUE_Seed)
plot(dp3.2$Period,dp3.2$CPUE_Legal)

# CPUE_Cat<-subset(dp3.2,dp3.2$Site =="Cat Point")
# CPUE_Hotel<-subset(dp3.2,dp3.2$Site =="Hotel Bar")
# CPUE_Dry<-subset(dp3.2,dp3.2$Site =="Dry Bar")
# CPUE_Bulkhead<-subset(dp3.2,dp3.2$Site =="Bulkhead")
# 
# f1<-ggplot(CPUE_Cat, aes(Period, CPUE_Legal)) +
#   geom_point(size=4) +
#   ggtitle("Cat Point Legal") +
#   xlim(0,14)+
#   xlab("Period") +
#   ylab("CPUE Legal")
# 
# f2<-ggplot(CPUE_Hotel, aes(Period, CPUE_Legal)) +
#   geom_point(size=4) +
#   ggtitle("Hotel Bar Legal") +
#   xlim(0,14)+
#   xlab("Period") +
#   ylab("CPUE Legal")
# 
# f3<-ggplot(CPUE_Dry, aes(Period, CPUE_Legal)) +
#   geom_point(size=4) +
#   xlim(0,14)+
#   ggtitle("Dry Bar Legal") +
#   xlab("Period") +
#   ylab("CPUE Legal")
# 
# f4<-ggplot(CPUE_Bulkhead, aes(Period, CPUE_Legal)) +
#   geom_point(size=4) +
#   xlim(0,14)+
#   ggtitle("Bulkhead") +
#   xlab("Period") +
#   ylab("CPUE Legal")
# 
# 
# plot_grid(f1,f2,f3,f4)
#####################################################

#ok this is a key plot below. Suggests
#that spat differ by study

#but one problem is DEP is not sampled until 2 years after
#cultch put out.

#change order that it plots in facet
dp3.2x <- dp3.2                              # Replicate data
dp3.2x$Project <- factor(dp3.2x$Project,      # Reordering group factor levels
                         levels = c("NFWF_1", "NRDA_4044", "NRDA_5007", "FWC_2021"))

spat_study<-ggplot(dp3.2x, aes(Period, CPUE_Spat)) +
  geom_point(size=2) +
  ggtitle("CPUE Spat by Period") +
  scale_x_continuous(breaks=seq(2,13,1))+
  xlab("Period") +
  ylab("CPUE Spat") +
  facet_wrap(~Project)

#ggsave("AB_spat_study.png", width = 10, height = 10)

seed_study<-ggplot(dp3.2, aes(Period, CPUE_Seed)) +
  geom_point(size=2) +
  ggtitle("Seed CPUE by Period") +
  xlab("Period") +
  ylab("Seed CPUE") +
  scale_x_continuous(breaks=seq(2,13,1))+
  facet_wrap(~Project)
ggsave("sub_study.png", width = 10, height = 10)

legal_study<-ggplot(dp3.2, aes(Period, CPUE_Legal)) +
  geom_point(size=2) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal CPUE") +
  facet_wrap(~Project)
ggsave("legal_study.png", width = 10, height = 10)



################
################
#moving on to GLM

names(dp3.2)
#[1] "Project"       "Period"        "Site"          "Sum_spat"      "Num_quads"    
#[6] "Sum_Seed"  "Sum_legal"     "CPUE_Spat"     "CPUE_Seed" "CPUE_Legal" 



qqnorm(dp3.2$Sum_spat)
qqnorm(dp3.2$Sum_Seed)
qqnorm(dp3.2$Sum_legal)

#yes overdispersed

#move back to counts not CPUE

#some GLMs

#single plot, sum spat
#this is the raw data, then you will fit nb.glm to these data 
#and use sampling site
#as random effect

##below key to show FWC-FSU

#this is by site and study
r0<-ggplot(dp3.2, aes(Period, Sum_spat,color=Project)) +
  geom_point(size=4) +
  ggtitle("Spat per Period") +
  xlab("Period") +
  scale_x_continuous(breaks=seq(2,13,1))+
  ylab("Total Spat")
#ggsave("apalach spat per period.png", width=10, height=10)

#+
#  facet_wrap(~Site)

#this is by study on one plot on log scale

# r0.1<-ggplot(dp3.2, aes(Period, Sum_spat,color=Project)) +
#   geom_point(size=4) +
#   ggtitle("Spat per Period by Site") +
#   xlab("Period") +
#   ylab("Total Spat")+
#   scale_y_log10()+
#   facet_wrap(~Site)
# #ggsave("SSSS.png", width=10, height=10)
# 
# 
# r1<-ggplot(data = dp3.2[dp3.2$Project=="NFWF_1",], aes(Period, Sum_spat)) +
#   geom_point(size=3) +
#   geom_point(data = dp3.2[dp3.2$Project=="NRDA_4044",], mapping = aes(Period, Sum_spat, color="red"), size = 3)+
#   geom_point(data = dp3.2[dp3.2$Project=="NRDA_5007",], mapping = aes(Period, Sum_spat, color="blue"), size = 3)+
#   ggtitle("Spat per Period by Study") +
#   scale_y_log10()+
#   xlab("Period") +
#   ylab("Total Spat")
#   #facet_wrap(~Project)

#this is by study on one plot not on log scale
r2<-ggplot(data = dp3.2[dp3.2$Project=="NFWF_1",], aes(Period, Sum_spat)) +
  geom_point(size=3) +
  geom_point(data = dp3.2[dp3.2$Project=="NFWF_1",], mapping = aes(Period, Sum_spat, color="NFWF 1"), size = 3)+
  geom_point(data = dp3.2[dp3.2$Project=="NRDA_4044",], mapping = aes(Period, Sum_spat, color="NRDA 4044"), size = 3)+
  geom_point(data = dp3.2[dp3.2$Project=="NRDA_5007",], mapping = aes(Period, Sum_spat, color="NRDA 5077"), size = 3)+
  geom_point(data = dp3.2[dp3.2$Project=="FWC_2021",], mapping = aes(Period, Sum_spat, color="FWC 2021"), size = 3)+
  ggtitle("Spat per Period by Study") +
  xlab("Period") +
  ylab("Total Spat")
#facet_wrap(~Project)

######################
######
#glmmTMB approach
######
######################

dp3.2$Site<-as.factor(dp3.2$Site)


library(glmmTMB)
library(bbmle)

#bring in a measure of discharge
#it isn't really discharge as CFS, it is number of days
#in a period below 12000 CFS @ JWLD

#there are two discharge files, one below 12000 CFS the other below 6000 CFS

Lowdays <- read.csv("~/Git/AB_DEP/below_12_threshold.csv")
dp4<-merge(dp3.2,Lowdays, by=c("Period"))
for(i in 1:nrow(dp4))
{dp4$lag1[i] <- Lowdays$Discharge[Lowdays$Period == (dp4$Period[i]-1)]}

names(dp4)

#plot(dp4$Sum_spat~dp4$Lowdays)

#####################
#Show this to FWC/FSU
#####################

names(dp4)[names(dp4) == 'Discharge'] <- 'Lowdays'

z1<- ggplot(dp4, aes(x=Lowdays, y=Sum_spat))+
  geom_point(size=3)+
  ylab("Live oyster spat count") +
  xlab ("Days discharge < 12000 CFS")+
  ggtitle("Live oyster spat count and days river < 12000 CFS") 

#ggsave("low days and spat.png", width=10, height=10)

z2<- ggplot(dp4, aes(x=Lowdays, y=CPUE_Spat))+
  geom_point(size=3)+
  ylab("Live oyster spat CPUE") +
  xlab ("Days discharge < 12000 CFS")+
  ggtitle("Live oyster spat CPUE and days discharge < 12000 CFS") 

#ggsave("low days and spat CPUE.png", width=10, height=10)

names(dp4)

#Period only
tmb0 <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb0)

#this model is asking how period and project influence counts
#using NB2 formulation (most common)
tmb1 <- glmmTMB(Sum_spat ~ Period * Project + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb1)

#so the NFWF 2021 project is intercept 

##don't forget to backtransform when looking at the summaries


#here is a way to generate CI on parameters
testci<-confint(tmb1)


################
#now compare multiple models from same family
#and add discharge
################

#do we really care about project? At the highest level no
#these sites nearly all received cultch
#we could look more closely at project later since they are different materials (?)

tmb4 <- glmmTMB(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb4)
#and what the model above tells us is that over time spat counts are declining

#now is that influenced by the number of low days?
#more low days would be drier conditions

tmb5 <- glmmTMB(Sum_spat ~ Period + Lowdays + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb5)
#and low days is significant and negative, so as 
#you have more low days (drier conditions) that has a negative
#effect on number of spat

#now with lag of 1 period on lowdays
tmb5.1 <- glmmTMB(Sum_spat ~ Period + lag1 + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb5.1)
#lag 1 not significant and AIC suggests no lag better fit also

#just low days
tmb6 <- glmmTMB(Sum_spat ~ Lowdays + (1|Site) + offset(log(Num_quads)), data = dp4, family="nbinom2") #converge
summary(tmb6)
#interesting just low days not significant
#
####Project is driving so much because the NFWF project
####is the only one with data during period of high counts
####DEP project was in the water, but monitoring wasn't done
####for several periods after building project


AICtab(tmb0, tmb1,tmb4,tmb5,tmb5.1,tmb6)
#This suggests project is really important from AIC
#but is it what we are interested in right now?
#maybe if we dig into what we can learn from each project

#just comparing models w/o project terms
AICtab(tmb4,tmb5)
#suggests including number of lowdays is important

library(ggeffects)

#make plots by study and period of obs and pred
#but with common slope
ggpredict(tmb1)
pred_tmb1 <- ggpredict(tmb1, c("Period", "Project"))
plot(pred_tmb1, facet=TRUE, colors=c("red","black","blue","orange"), add.data=TRUE)


########
#######ggpredict to predict for 1 period and quadrat
#https://cran.r-project.org/web/packages/ggeffects/ggeffects.pdf


##Jennifer approach updated March 30


#predict all projects 
new.dat = data.frame(Sum_spat = dp4$Sum_spat,
                     Period = dp4$Period,
                     Num_quads = dp4$Num_quads)

new.tmb1 <- glmmTMB(Sum_spat ~ Period + offset(log(Num_quads)), data = new.dat, family="nbinom2") #converge

ggpredict(new.tmb1)
test1 = ggpredict(new.tmb1, terms = c("Period[13]", "Num_quads[1]"), type = c('fe')) #in draft
test1.1 = ggpredict(new.tmb1, terms = c("Period[1]", "Num_quads[1]"), type = c('fe')) #in draft


############
##this is a decent one to show how predict works, run test2
##compare to Berrigan 1990 Table 1

#below with project

new.dat2 = data.frame(Sum_spat = dp4$Sum_spat,
                     Period = dp4$Period,
                     Project = dp4$Project,
                     Num_quads = dp4$Num_quads)

new.tmb2 <- glmmTMB(Sum_spat ~ Period * Project + offset(log(Num_quads)), data = new.dat2, family="nbinom2") #converge

ggpredict(new.tmb2)
test2 = ggpredict(new.tmb2, terms = c("Period", "Project", "Num_quads[1]"), type = c('fe')) #for all projects


#below is for one project

#this is a decent example of model fit to data. About 150 quads done each time in NFWF1
#this is an example for folks to review

test3 = ggpredict(new.tmb2, terms = c("Period", "Project[NFWF_1]","Num_quads[150]"), type = c('fe')) #for one project
plot(test3, facet=FALSE, add.data=TRUE)
#ggsave("pred_apalach_150quad.png", width=10, height=10)

  
#now let's predict for last period = 13, by study, for 1 quad
#this is in the paper
unique(new.dat2$Project)
#"NFWF_1"    "NRDA_4044" "NRDA_5007" "FWC_2021" 

ggpredict(new.tmb2)
test4.nfwf1 = ggpredict(new.tmb2, terms = c("Period[13]", "Project[NFWF_1]", "Num_quads[1]"), type = c('fe')) 
test4.nrda4044 = ggpredict(new.tmb2, terms = c("Period[13]", "Project[NRDA_4044]", "Num_quads[1]"), type = c('fe')) 
test4.nrda5077 = ggpredict(new.tmb2, terms = c("Period[13]", "Project[NRDA_5007]", "Num_quads[1]"), type = c('fe')) 
test4.fwc2021 = ggpredict(new.tmb2, terms = c("Period[13]", "Project[FWC_2021]", "Num_quads[1]"), type = c('fe')) 


               
#this is just lowdays (not significant on its own)

new.dat3 = data.frame(Sum_spat = dp4$Sum_spat,
                      Period = dp4$Period,
                      Lowdays = dp4$Lowdays,
                      Num_quads = dp4$Num_quads)

new.tmb3 <- glmmTMB(Sum_spat ~ Period + Lowdays + offset(log(Num_quads)), data = new.dat3, family="nbinom2") #converge

ggpredict(new.tmb3)
test3 = ggpredict(new.tmb3, terms = c("Period[13]", "Num_quads[1]"), type = c('fe')) #for all projects
#this is for the average number of low days.




#plot(pred_tmb5, facet=TRUE, colors=c("red","black","blue"), add.data=TRUE)

#neat that works

#now make the Jennifer style plot with group and period
##this is in the paper, figure 6 i think

nfwf_pred<- subset(test2, test2$group == "NFWF_1")
DEP_4044<- subset(test2, test2$group == "NRDA_4044")
DEP_5007<- subset(test2, test2$group == "NRDA_5007")
FWC_2021<- subset(test2, test2$group == "FWC_2021")
#FWC_2021<- subset(test2, pred_tmb1$group == "FWC_2021")
#this one on 606 is how it was originally called in the draft. Not sure why
#it wasn't called from test2$group

pr1 = ggplot(nfwf_pred, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Live oyster per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
    ggtitle("NFWF Apalachicola Spat by Period") +
  #geom_point(data = dp3.2[dp3.2$Project == "NFWF_1",], mapping = aes(Period, Sum_spat), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))
#+
#  scale_y_continuous(breaks=seq(0,100000,1000))

pr2 = ggplot(DEP_4044, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Live oyster per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("DEP 4044 Spat by Period") +
  #geom_point(data = dp3.2[dp3.2$Project == "NRDA_4044",], mapping = aes(Period, Sum_spat), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))

pr3 = ggplot(DEP_5007, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Live oyster per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("DEP 5007 Spat by Period") +
  #geom_point(data = dp3.2[dp3.2$Project == "NRDA_5007",], mapping = aes(Period, Sum_spat), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))

pr4 = ggplot(FWC_2021, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Live oyster per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  ggtitle("FWC 2021 Spat by Period") +
  #geom_point(data = dp3.2[dp3.2$Project == "FWC_2021",], mapping = aes(Period, Sum_spat), size = 2)+
  scale_x_continuous(breaks=seq(1,14,1))


plot_grid(pr1,pr2,pr3,pr4)
#ggsave("pred_apalach_1quad.png", width=10, height=10)

####END####

############
#now Jennifer style but just period. this is what
#is out there

p_pr1 = ggplot(pred_tmb4, aes(x, predicted))+
  geom_line(size=2)+
  ylab("Live oyster count per quad") +
  xlab ("Period")+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5) +
  geom_point(data = dp4, mapping = aes(Period, Sum_spat), size = 2)+
  ggtitle("Spat by Period") +
  scale_x_continuous(breaks=seq(1,14,1))
#+
#  scale_y_log10()
#+
#  scale_y_continuous(breaks=seq(0,100000,1000))





########################

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
