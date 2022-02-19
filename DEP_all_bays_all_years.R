
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

ggsave("dep_allbays.pdf", width = 10, height = 10)
#############

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

names(d6)

qqnorm(d6$Sum_spat)
qqnorm(d6$Sum_sublegal)
qqnorm(d6$Sum_legal)

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
m3 <- glm.nb(Sum_spat ~ Bay * Period + offset(log(Num_quads)),data = d6, control = glm.control(maxit = 500)
) 

cand.set = list(m1,m2,m3)
modnames = c("bay", "bay+period", "bay*period")
aictab(cand.set, modnames, second.ord = FALSE) #model selection table with AIC

summary(m3)

m1.1 <- glm.nb(Sum_sublegal ~ Bay + offset(log(Num_quads)), data = d6) 
m2.1 <- glm.nb(Sum_sublegal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
m3.1 <- glm.nb(Sum_sublegal ~ Bay * Period + offset(log(Num_quads)), data = d6) 

cand.set2 = list(m1.1,m2.1,m3.1)
modnames2 = c("bay", "bay+period", "bay*period")
aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC

summary(m1.1)

#legals
m1.2 <- glm.nb(Sum_legal ~ Bay + offset(log(Num_quads)), data = d6) 
m2.2 <- glm.nb(Sum_legal ~ Bay + Period + offset(log(Num_quads)), data = d6) 
m3.2 <- glm.nb(Sum_legal ~ Bay * Period + offset(log(Num_quads)), data = d6) 

cand.set3 = list(m1.2,m2.2,m3.2)
modnames3 = c("bay", "bay+period", "bay*period")
aictab(cand.set3, modnames3, second.ord = FALSE) #model selection table with AIC

summary(m2.2)

control = glm.control(maxit = 500)

#plot ideas
https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html
#https://mran.microsoft.com/snapshot/2017-04-22/web/packages/sjPlot/vignettes/sjpglm.html
https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/plotting-functions-ggplotsci.html#estimate-response-and-effects-with-emmeans
