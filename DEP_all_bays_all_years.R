
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

library(tidyr)



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

#now remove the 3 or 4 NA's from Pensacola (lost samples?)
d4 <- filter(d4, Legal != "NA")

names(d4)
#check bays
unique(d4$Bay)
#check sites
unique(d4$Site)

#just check to see if other NAs
which(is.na(d4$SubLegal), arr.ind=TRUE)

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
# 
# ##summary number of oyster spat counted each period
# spat_sum <- d4 %>%
#   dplyr::group_by(Bay, Period) %>%
#   dplyr::summarise(sum=sum(Spat,na.rm=TRUE)) %>%
#   dplyr::arrange(Bay, Period)
# 
# names(spat_sum) <- c("Bay","Period",
#                      "Number Live Spat")

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
count_spat=aggregate(Spat~Bay+Period,data=d4,sum)
count_spat <- dplyr::rename(count_spat,Bay=Bay,Period=Period,Sum_spat=Spat)

count_sublegal=aggregate(Sublegal~Bay+Period,data=d4,sum)
count_sublegal <- dplyr::rename(count_sublegal,Bay=Bay,Period=Period,Sum_sublegal=Sublegal)

count_legal=aggregate(Legal~Bay+Period,data=d4,sum)
count_legal <- dplyr::rename(count_legal,Bay=Bay,Period=Period,Sum_legal=Legal)

#count number quads by doing the length of transect, then rename
count_quads=aggregate(Spat~Bay+Period,data=d4,length)
count_quads_spat <- dplyr::rename(count_quads,Bay=Bay,Period=Period,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d5=merge(count_spat,count_quads_spat,by=c("Bay", "Period"))
d5.1=merge(d5,count_sublegal,by=c("Bay", "Period"))
d5.2=merge(d5.1,count_legal,by=c("Bay", "Period"))

d6<-d5.2

#calculate CPUE. Just for fun to plot
d6$CPUE_Spat<-d6$Sum_spat/d6$Num_quads
d6$CPUE_Sublegal<-d6$Sum_sublegal/d6$Num_quads
d6$CPUE_Legal<-d6$Sum_legal/d6$Num_quads

f1<-ggplot(d6, aes(Period, CPUE_Spat)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

f2<-ggplot(d6, aes(Period, CPUE_Sublegal)) +
  geom_point(size=4) +
  ggtitle("Sublegal CPUE by Period") +
  xlab("Period") +
  ylab("Sublegal") +
  facet_wrap(~Bay)

f3<-ggplot(d6, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(f1,f2,f3)

#ggsave("dep_allbays_cpue.pdf", width = 10, height = 10)

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

f5<-ggplot(d6, aes(Period, Sum_sublegal)) +
  geom_point(size=4) +
  ggtitle("Sublegal Sum by Period") +
  xlab("Period") +
  ylab("Sublegal") +
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

#fit basic NB GLM
m1 <- glm.nb(Sum_spat ~ Bay + offset(log(Num_quads)), data = d6) 
m2 <- glm.nb(Sum_spat ~ Bay + Period + offset(log(Num_quads)), data = d6) 
m3 <- glm.nb(Sum_spat ~ Bay * Period + offset(log(Num_quads)),data = d6, control = glm.control(maxit = 5000)
) 

cand.set = list(m1,m2,m3)
modnames = c("bay", "bay+period", "bay*period")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

#so model 3 isn't fitting
summary(m1)
summary(m2)

#results suggests for spat bays are different but period isn't

m1.1 <- glm.nb(Sum_sublegal ~ Bay + offset(log(Num_quads)), data = d6) 
m2.1 <- glm.nb(Sum_sublegal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
m3.1 <- glm.nb(Sum_sublegal ~ Bay * Period + offset(log(Num_quads)), data = d6) 

cand.set2 = list(m1.1,m2.1,m3.1)
modnames2 = c("bay", "bay+period", "bay*period")
aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC

summary(m2.1)
#again, bays differ for sublegal but time doesn't

#legals
m1.2 <- glm.nb(Sum_legal ~ Bay + offset(log(Num_quads)), data = d6) 
m2.2 <- glm.nb(Sum_legal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
m3.2 <- glm.nb(Sum_legal ~ Bay * Period + offset(log(Num_quads)), data = d6) 

cand.set3 = list(m1.2,m2.2,m3.2)
modnames3 = c("bay", "bay+period", "bay*period")
aictab(cand.set3, modnames3, second.ord = FALSE) #model selection table with AIC

summary(m2.2)
#for legals, st. andrews differs from others but that's it

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
count_spat_site=aggregate(Spat~Bay+Period+Site,data=d4,sum)
count_spat_site <- dplyr::rename(count_spat_site,Bay=Bay,Period=Period,Sum_spat=Spat, Site=Site)

count_sublegal_site=aggregate(Sublegal~Bay+Period+Site,data=d4,sum)
count_sublegal_site <- dplyr::rename(count_sublegal_site,Bay=Bay,Period=Period,Sum_sublegal=Sublegal,Site=Site)

count_legal_site=aggregate(Legal~Bay+Period+Site,data=d4,sum)
count_legal_site <- dplyr::rename(count_legal_site,Bay=Bay,Period=Period,Sum_legal=Legal, Site=Site)

#count number quads by doing the length of transect, then rename
count_quads_site=aggregate(Spat~Bay+Period+Site,data=d4,length)
count_quads_site <- dplyr::rename(count_quads_site,Bay=Bay,Period=Period,Num_quads=Spat)

#merge spat live count total data frame with the tran_length total data frame
d7=merge(count_spat_site,count_quads_site,by=c("Bay", "Period", "Site"))
d7.1=merge(d7,count_sublegal_site,by=c("Bay", "Period", "Site"))
d7.2=merge(d7.1,count_legal_site,by=c("Bay", "Period", "Site"))

d8<-d7.2


names(d8)

d9<-dplyr::select(d8,Bay, Period, Site, Num_quads, 
               Sum_spat,Sum_sublegal,Sum_legal)

#calculate CPUE. Just for fun to plot
d9$CPUE_Spat<-d9$Sum_spat/d9$Num_quads
d9$CPUE_Sublegal<-d9$Sum_sublegal/d9$Num_quads
d9$CPUE_Legal<-d9$Sum_legal/d9$Num_quads

#the plots below show you the range in CPUE for a given size class
#of oyster in each of the bays

s1<-ggplot(d9, aes(Period, CPUE_Spat)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat") +
  facet_wrap(~Bay)

s2<-ggplot(d9, aes(Period, CPUE_Sublegal)) +
  geom_point(size=4) +
  ggtitle("Sublegal CPUE by Period") +
  xlab("Period") +
  ylab("Sublegal") +
  facet_wrap(~Bay)

s3<-ggplot(d9, aes(Period, CPUE_Legal)) +
  geom_point(size=4) +
  ggtitle("Legal CPUE by Period") +
  xlab("Period") +
  ylab("Legal") +
  facet_wrap(~Bay)

plot_grid(s1,s2,s3)

ggsave("dep_allbays_site.pdf", width = 10, height = 10)

#single plot of CPUE, spat, different color by Bay
s4<-ggplot(d9, aes(Period, CPUE_Spat, color=Bay)) +
  geom_point(size=4) +
  ggtitle("Spat CPUE by Period") +
  xlab("Period") +
  ylab("Spat CPUE")

#########
##ok move away from CPUE and work on the GLMs
#########

qqnorm(d9$Sum_spat)
qqnorm(d9$Sum_sublegal)
qqnorm(d9$Sum_legal)

#yes overdispersed

#some GLMs

#single plot, sum spat
#this is the raw data, then you will fit nb.glm to these data 
#and use sampling site
#as random effect

r0<-ggplot(d9, aes(Period, Sum_spat)) +
  geom_point(size=4) +
  ggtitle("Spat per Period by Site") +
  xlab("Period") +
  ylab("Total Spat")+
  facet_wrap(~Bay)


#fit basic NB GLM Random

library(lme4) #mixed effect models
library(MASS) #negative binomial models

#make site factor for random effect
d9$Site<-as.factor(d9$Site)

#make bay factor to use in ggpredict as group
d9$Bay<-as.factor(d9$Bay)

#no offset, station name as random
r1 <- glmer.nb(Sum_spat ~ Period + (1|Site) + offset(log(Num_quads)), data = d9) #no converge
r2 <- glmer.nb(Sum_spat ~ Period + Bay + (1|Site) + offset(log(Num_quads)), data = d9) #converge

summary(r2)

library(ggeffects)

ggpredict(r2)

pred_r2 <- ggpredict(r2, c("Period", "Bay"))
#really important to look at this pred_r2 because it shows you
#the groups have unique predictions

#plot(pred_r2, facet=TRUE, colors="Bay")
plot(pred_r2, facet=TRUE, colors=c("red","black","blue"))

#plot(pred_r2, facet=TRUE, colors="social")
#palletts and options here
#https://strengejacke.github.io/ggeffects/reference/plot.html#examples

#or a better way is how Jennifer does it

#now subset the predicted for each bay
app_pred <- subset(pred_r2, pred_r2$group == "Apalachicola")
pen_pred <- subset(pred_r2, pred_r2$group == "Pensacola")
sa_pred <- subset(pred_r2, pred_r2$group == "St. Andrews")

#now make new plots for each bay and add the raw data points
#this gives you predicted for each bay and then the raw data points

pm1 = ggplot(app_pred, aes(x, predicted))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+
  ylab("Live oyster count per quad") +
  xlab ("Period")+
  ggtitle("Apalachicola Spat by Period") +
  geom_point(data = d9[d9$Bay == "Apalachicola",], mapping = aes(Period, Sum_spat), size = 2)

  
pm2 = ggplot(pen_pred, aes(x, predicted))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+
  ylab("Live oyster count per quad") +
  xlab ("Period")+
  ggtitle("Pensacola Spat by Period") +
  geom_point(data = d9[d9$Bay == "Pensacola",], mapping = aes(Period, Sum_spat), size = 2)

pm3 = ggplot(sa_pred, aes(x, predicted))+
  geom_line(size=2)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .5)+
  ylab("Live oyster count per quad") +
  xlab ("Period")+
  ggtitle("St. Andrews Spat by Period") +
  geom_point(data = d9[d9$Bay == "St. Andrews",], mapping = aes(Period, Sum_spat), size = 2)

plot_grid(r0,pm1,pm2,pm3)

ggsave("dep_allbays_predicted_nbglm.pdf", width = 10, height = 10)

#or

pm <- plot_grid(
  pm1 + theme(legend.position="none"),
  pm2 + theme(legend.position="none"),
  pm3 + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 1,
  ncol=3
)


#for plotting could scale y axis better but that would cause 1 point for St. Andrews to be lost
  

  
  
  
# 
# #plot ideas
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
# #https://mran.microsoft.com/snapshot/2017-04-22/web/packages/sjPlot/vignettes/sjpglm.html
# https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/plotting-functions-ggplotsci.html#estimate-response-and-effects-with-emmeans
