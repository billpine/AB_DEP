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
library(ggplot2); theme_set(theme_bw(base_size=16) + theme(panel.spacing = grid::unit(0, "lines")))
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(AICcmodavg)
library(emmeans)
library(DHARMa)
library(readr)
library(jtools)


#if you want to only do the rock analyses with Apalach then you can
#subset for Apalach here. Else make d1 the full data file with all bays
#d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")
d0 <- read.csv("~/Git/AB_DEP/20220326_merged_agency_data.csv")

#ok, change Apalachicola Bay to Apalachicola
d0.1<-d0 %>%
  mutate(Bay = replace(Bay,Bay == "Apalachicola Bay", "Apalachicola"))%>%
  mutate(Project = replace(Project,Project =="FWC-2021","NFWF-2021"))

#d1<- d0.1

d1<- subset(d0.1, d0.1$Bay == "Apalachicola")

#the FWC data have been modified per Matt at FWC to
#do the proportions based on size for the number per size category

#switch -999 to NA instead of removing
d2 <- d1
d2$Spat[d2$Spat < -1] <- NA
d2$Weight[d2$Weight < -1] <- NA
d2$Weight_kg[d2$Weight_kg < -1] <- NA
d2$Seed[d2$Seed < -1] <- NA
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

#sum wt for each site period season
wt=aggregate(Weight~Bay+Project+Site+Period,data=d2,sum)
sum_wt <- dplyr::rename(wt, Bay=Bay,Project=Project, Site=Site,Period=Period, Weight=Weight)
names(sum_wt) <- c("Bay", "Project", "Site", "Period", "Wt_sum")

#sum spat for each site period season
spat=aggregate(Spat~Bay+Project+Site+Period,data=d2,sum)
sum_spat <- dplyr::rename(spat, Bay=Bay,Project=Project, Site=Site,Period=Period, Sum_spat=Spat)
names(sum_spat) <- c("Bay","Project", "Site", "Period", "Spat_sum")

#count number quads by doing the length of aggregated quads, then rename
count_quads=aggregate(Weight~Bay+Project+Site+Period,data=d2,length)
count_quads_weight <- dplyr::rename(count_quads,Site=Site,Period=Period, Num_quads=Weight)

#merge weight total data frame with the num quads total data frame and spat total
d2.9=merge(sum_wt, sum_spat,by=c("Bay", "Project","Site", "Period"))
#merge weight total data frame with the num quads total data frame and spat total
d3=merge(d2.9, count_quads_weight,by=c("Bay", "Project","Site", "Period"))


#this is by study on one plot, then wrapped by Bay 
r1<-ggplot(data = d3[d3$Bay=="Apalachicola",], aes(x=Wt_mean, y=Spat_mean, color=Project)) +
  geom_point(size=3)+
  #ggtitle("Apalachicola Mean Cultch Weight vs. Mean Number Spat Per Quad") +
  xlab("Mean weight (kg)") +
  ylab("Mean spat")+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  facet_wrap(~Site)

#ggsave("meanwt_meanspat.png", width=10, height=10)



###ok now just plot the raw spat and raw weights, not the mean
names(d2)

r2<-ggplot(data = d2[d2$Bay=="Apalachicola",], aes(x=Weight, y=Spat, color=Project, na.rm=TRUE)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  xlab("Cultch weight (kg)") +
  ylab("Spat")+
  facet_wrap(~Site)
#ggsave("r2_rawspat_rawweight_site.png", width=10, height=10)


r3<-ggplot(data = d2[d2$Bay=="Apalachicola",], aes(x=Weight, y=Spat, color=Project, na.rm=TRUE)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black", "light blue", "red", "dark blue"))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2))+
  xlab("Cultch weight (kg)") +
  ylab("Spat")+
  facet_wrap(~Period)
#ggsave("r3_rawspat_rawweight_period.png", width=10, height=10)

  
plot_grid(r2, r3, labels = c('A', 'B'))

#ggsave("rawwt_rawspat_site_period.png", width=10, height=10)


###


library(glmmTMB)
library(bbmle)


names(d3)

head(d3)



#round weight to make it integer
d3$Roundwt<-round(d3$Wt_sum,0)

as.integer(d3$Roundwt)

## BMB: the same but 'looks' like an interaction
d3$SP <- with(d3, interaction(Site, Project, sep = "_", drop = TRUE))


dApalach<-subset(d3,d3$Bay =="Apalachicola")

plot(dApalach$Spat_sum~dApalach$Roundwt)


tab <- with(dApalach,table(SP,Project))
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

######

plot(dApalach$Spat_sum~dApalach$Roundwt)
m1<-lm(Spat_sum~Roundwt, data=dApalach)
summary(m1)


ggplot(dApalach, aes(x=predict(m1), y= Spat_sum)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Round weight', y='Spat sum', title='')



#########
#########
#WARNING A BUNCH OF THESE MODELS ARE LIKELY OVERPARAMETERIZED
#########
#########







#Intercept
tmb0.AB <- glmmTMB(Roundwt ~ (1|SP) + offset(log(Num_quads)),
                   data = dApalach, family="nbinom2") #converge
summary(tmb0.AB)


ggplot(dApalach, aes(x=predict(tmb0.AB), y= Spat_sum)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Round weight', y='Spat sum', title='')


#spat sum

tmb00.AB <- update(tmb0.AB, . ~ . + Spat_sum)
summary(tmb00.AB)


#Period
tmb1.AB <- update(tmb00.AB, . ~ . + Period)
summary(tmb1.AB)

#Period + Project
tmb2.AB <- update(tmb1.AB, . ~ . + Project)
summary(tmb2.AB)


#Period*Project
tmb3.AB <- update(tmb2.AB, . ~ . + Period:Project)
summary(tmb3.AB)


########################

#Project
tmb4.AB <- update(tmb0.AB, . ~ . + Project)
summary(tmb4.AB)

#Ben note below
## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC

#bill note on tmb5. Ben removes the random effect term on site and then nests it
#period

#ben note below
#Period*bay/site (allow period by site across project)
tmb5.AB <- update(tmb3.AB, . ~ . - (1|SP) + (Period|SP))
diagnose(tmb5.AB)  ##  BMB: this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5.AB)

summary(tmb5.AB)

#plot coefficients
plot_summs(tmb5.AB)

#all + project:Spat_sum
#i think this is what ed wants to see (but I think way overparameterized, the SE are too small)
tmb5.AB.xx <- update(tmb3.AB, . ~ . - (1|SP) - Spat_sum + Project:Spat_sum + (Period|SP)) #w/o spat sum

diagnose(tmb5.AB.xx)  ##  BMB: this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5.AB)


summary(tmb5.AB.xx)
(em5.ABxx <- emtrends(tmb5.AB.xx, ~Project, "Spat_sum"))
test(em5.ABxx)


#all + dispersion
tmb6.AB <- update(tmb5.AB, dispformula = ~Project)
                  summary(tmb6.AB)

#bp note, tmb6.AB is tmb5.AB + adding a unique dispersion parameter for each project. 
#has singular convergence issue goes away with bfgs 

tmb7.AB <- update(tmb3.AB, . ~ .  + (0+Period|SP), dispformula = ~Project)
summary(tmb7.AB)

diagnose(tmb7.AB)
VarCorr(tmb7.AB)

#model selection information

## self-naming list
cand.set2.AB =
  list(tmb0.AB, tmb00.AB, tmb1.AB, tmb2.AB, tmb3.AB, tmb4.AB, tmb5.AB, tmb5.AB.xx, tmb6.AB, tmb7.AB)
modnames2.AB = c("intercept","spat sum", "period", "period + project", "period*project", "project", "all",
                 "all +project:spat_sum","all + dispersion", "project/sp uncorr + disp")
names(cand.set2.AB) <- modnames2.AB


#AIC
aictab(cand.set2.AB, modnames2.AB, second.ord = FALSE) #model selection table with AIC
#AICc
aictab(cand.set2.AB, modnames2.AB, second.ord = TRUE) #model selection table with AICc

#for example, grab tmb5 and fit it w/o spat sum and see if different

tmb5.AB.x <- update(tmb3.AB, . ~ . - (1|SP) - Spat_sum + (Period|SP)) #w/o spat sum
AIC(tmb5.AB.x,tmb5.AB) #delta AIC about 1.8. so not separable (w/o is slightly lower)


AIC(tmb5.AB.xx,tmb5.AB) #still no improvement


# #now take all and add spat?
# 
# tmb3.AB.spat <- update(tmb2.AB, . ~ . + Period:Project + Spat_sum)
# summary(tmb3.AB.spat)
# 
# plot_summs(tmb3.AB.spat,tmb3.AB)
# 
# tmb5.AB.spat <- update(tmb3.AB, . ~ . - (1|SP) + (Period|SP)+Spat_sum)
# summary(tmb5.AB.spat)
# 
# AIC(tmb5.AB.spat,tmb5.AB)
# 
# ## quantify and test trends by project

# 
# (em3.AB.spat <- emtrends(tmb3.AB.spat, ~Project, "Period"))
# test(em3.AB.spat)

