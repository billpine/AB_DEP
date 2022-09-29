#20220919
#data are counts of oysters in three Florida bays
#spat are <25 mm
#seed 25-74 mm
#legal 75+mm

#key interest is whether oyster counts change over
#time and by bay

setwd("~/Fred")

library(ggplot2); theme_set(theme_bw(base_size=16) + theme(panel.spacing = grid::unit(0, "lines")))
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(AICcmodavg)
library(emmeans)

d5 <- read.csv("d5.csv")

d5$Bay<-as.factor(d5$Bay)

#Approach 1
#Site as random effect (one random effect for all 3 bays)
#effort controlled as offset (log num quads)
tab <- with(d5,table(Site,Bay))
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

#Intercept
tmb0 <- glmmTMB(Sum_spat ~ (1|Site) + offset(log(Num_quads)),
                data = d5, family="nbinom2") #converge
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

#full version of tmb5 written out by bp (to make sure I follow shorthand)

tmb5.1 <- glmmTMB(Sum_spat ~ Period + Bay + (Period|Site) + Period:Bay + offset(log(Num_quads)),
                data = d5, family="nbinom2") #converge
summary(tmb5.1)
##back to Ben's code

#all + dispersion
tmb6 <- update(tmb5, dispformula = ~Bay)

#ben original plot below has an error in y axis scale
#ggplot(d5, aes(Period, Sum_spat)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
#    scale_y_continuous(trans = log1p_trans())

ggplot(d5, aes(Period, Sum_spat)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())


## self-naming list
cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4, tmb5, tmb6)
modnames2 = c("intercept", "period", "period + bay", "period*bay", "bay", "period*bay/site",
              "all + dispersion")
names(cand.set2) <- modnames2

#AIC(c) table of all models
AICctab(cand.set2, weights=TRUE)
## best model is 'full' model, but without dispersion

#look into Fred's comments about glmmTMB and additive effects

## quantify and test trends by bay
(em1 <- emtrends(tmb5, ~Bay, "Period"))
test(em1)

#ok bp wants to compare to the bay*period model
(em3 <- emtrends(tmb3, ~Bay, "Period"))
test(em3)


aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC

############################
############################
############################
############################

############
#now Apalachicola Bay only to examine different projects within this bay
#remember the other bays only have one project

dp4 <- read.csv("dp4.csv")

##dp4$Site<-as.factor(dp4$Site)
##dp4$Project<-as.factor(dp4$Project)


##Apalachicola


#https://cran.r-project.org/web/packages/glmmTMB/vignettes/glmmTMB.pdf

#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#convergence-warnings


head(dp4)

names(dp4)


#below create new variable which makes unique sites by project to follow
#Ben's recommendation below
#Most of the software that can handle both crossed and nested random effects 
#can automatically detect when a nested model is appropriate, provided that the 
#levels of the nested factor are uniquely labeled. That is, the software can 
#only tell individuals are nested if they are labeled as A1, A2, . . . , 
#A10, B1, B2, . . . B10, . . . If individuals are instead identified only 
#as 1, 2, . . . 10 in each of species A, B, and C, the software can’t tell 
#that individual #1 of species A is not related to individual #1 of species B. 
#In this case you can specify nesting explicitly, but it is safer to label the 
#nested individuals uniquely.

dp4$SP <- paste(dp4$Site, dp4$Project, sep = "_")

#so now use SP instead of site

#check this with a graphic

tab <- with(dp4,table(SP,Project))
library(Matrix)
ifun <- function(M) {
  M <- as(M, "Matrix")
  image(M, aspect = "fill",
        scales = list(y = list(at = seq(nrow(M)), labels = rownames(M)),
                      x = list(at = seq(ncol(M)), labels = colnames(M), rot = 90)))
}
ifun(tab)

###########

## sum-to-zero contrasts so main effect of Period = unweighted average across Bays
options(contrasts = c("contr.sum", "contr.poly"))

#Intercept
tmb0.AB <- glmmTMB(Sum_spat ~ (1|SP) + offset(log(Num_quads)),
                data = dp4, family="nbinom2") #converge
summary(tmb0.AB)


#Period
tmb1.AB <- update(tmb0.AB, . ~ . + Period)
summary(tmb1.AB)

#Period + Project
tmb2.AB <- update(tmb1.AB, . ~ . + Project)
summary(tmb2.AB)

#Period*Project
tmb3.AB <- update(tmb2.AB, . ~ . + Period:Project)
summary(tmb3.AB)

#Project
tmb4.AB <- update(tmb0.AB, . ~ . + Project)
summary(tmb4.AB)

## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC

#bill note on tmb5. Ben removes the random effect term on site and then nests it
#period

#Period*bay/site (allow period by site across project)
tmb5.AB <- update(tmb3.AB, . ~ . - (1|SP) + (Period|SP))

summary(tmb5.AB)

#full version of tmb5.AB written out by bp (to make sure I follow shorthand)

glmmTMBControl(optCtrl=list(iter.max=1e5,eval.max=1e5))

tmb5.1.AB <- glmmTMB(Sum_spat ~ Period + Project + (Period|SP) + Period:Project + offset(log(Num_quads)),
                  data = dp4, family="nbinom2") 
summary(tmb5.1.AB)


##back to Ben's code

#all + dispersion
tmb6.AB <- update(tmb5.AB, dispformula = ~Project)

#ben original plot below has an error in y axis scale
#ggplot(dp4, aes(Period, Sum_spat)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
#    scale_y_continuous(trans = log1p_trans())

ggplot(dp4, aes(Period, Sum_spat)) + facet_wrap(~Project) + geom_point() + geom_line(aes(group = SP), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())


## self-naming list
cand.set2.AB = list(tmb0.AB,tmb1.AB,tmb2.AB,tmb3.AB,tmb4.AB, tmb5.AB, tmb6.AB)
modnames2.AB = c("intercept", "period", "period + project", "period*project", "project", "period*project/sp",
              "all + dispersion")
names(cand.set2.AB) <- modnames2.AB

#AIC(c) table of all models
AICctab(cand.set2.AB, weights=TRUE)
## best model is 'full' model, but without dispersion

#look into Fred's comments about glmmTMB and additive effects

## quantify and test trends by project
(em1.AB <- emtrends(tmb5.AB, ~Project, "Period"))
test(em1.AB)

#ok bp wants to compare to the project*period model
(em3.AB <- emtrends(tmb3.AB, ~Project, "Period"))
test(em3.AB)


aictab(cand.set2.AB, modnames2.AB, second.ord = FALSE) #model selection table with AIC

