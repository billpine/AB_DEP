#20220919
#data are counts of oysters in three Florida bays
#spat are <25 mm
#seed 25-74 mm
#legal 75+mm

#key interest is whether oyster counts change over
#time and by bay


#remember model specification table here https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

## setwd("~/Fred")

library(ggplot2); theme_set(theme_bw(base_size=16) + theme(panel.spacing = grid::unit(0, "lines")))
library(glmmTMB)
library(bbmle)
library(ggeffects)
library(AICcmodavg)
library(emmeans)
library(DHARMa)
library(readr)
library(jtools)

d5 <- read_csv("d5.csv")

d5$Bay <- as.factor(d5$Bay)
str(d5)

#############################
#Approach 1
#Site as random effect (one random effect for all 3 bays)
#effort controlled as offset (log num quads)
##############################

#Ben's neat table

tab <- with(d5,table(Site,Bay))
library(Matrix)
ifun <- function(M) {
    M <- as(M, "Matrix")
    image(M, aspect = "fill",
          scales = list(y = list(at = seq(nrow(M)), labels = rownames(M)),
                        x = list(at = seq(ncol(M)), labels = colnames(M), rot = 90)))
}
ifun(tab)

#ben inserted this, treatment contrasts to compare each group to the first group
#this may be important for interpreting the beta's because see how the main effect
#is the unweighted average across bays?
#update, i've tried it both ways,still don't see difference

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

#now model tmb6
#all + dispersion
#bp note, this is tmb5 + unique NB dispersion parameter for each Bay

tmb6 <- update(tmb5, dispformula = ~Bay)
summary(tmb6)

#this is Ben's plot of the data
ggplot(d5, aes(Period, Sum_spat)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())

#ok now let's look at the AIC (or are they AICc) tables created by Ben

cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4, tmb5, tmb6)
modnames2 = c("intercept", "period", "period + bay", "period*bay", "bay", "period*bay/site",
              "all + dispersion")
names(cand.set2) <- modnames2

#bp: hmm, see below, these are AICc tables.

#AIC(c) table of all models
AICctab(cand.set2, weights=TRUE)
## best model is 'full' model, but without dispersion
## This is Ben's comment above, what he means is best is
## the best is tmb5, which is same as tmb6, but tmb6 includes
## unique dispersion parameter for each Bay

#model selection information

aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
aictab(cand.set2, modnames2, second.ord = TRUE) #model selection table with AICc


#check autocorrelation issues
res1 <- simulateResiduals(tmb5)
plot(res1)
agg.res1 = recalculateResiduals(res1,group=d5$Period)
time = unique(dp4$Period)
plot(time,agg.res1$scaledResiduals,pch=16)



## quantify and test trends by bay
#em notes https://bookdown.org/dereksonderegger/571/4-contrasts.html
#https://cran.r-project.org/web/packages/emmeans/emmeans.pdf

#report this info below in paper.
#these are the beta values for each bay, not marginal means

(em1 <- emtrends(tmb5, ~Bay, "Period"))
test(em1)
##need to compare this to summary(tmb5) to check betas. st andrew weird, i can match the other
##two between emtrends results and summary(tmb5)

#> -0.05967+0.05610
#[1] -0.00357

#> -0.05967+-0.32938
#[1] -0.38905


#marginal means for tmb5 (this would be averaged across Period and effort)
mm.tmb5<-emmeans(tmb5, ~Bay, "Period")

pred.tmb5<- emmeans(tmb5, specs=~Bay, at=list(Period=c(15)))

#use this in paper
#or go back to ggeffects and make the prediction for period 15 for 1 quad
library(ggeffects)
ggpredict(tmb5)
pred_tmb5 <- ggpredict(tmb5, c("Period[15]", "Bay","Num_quads[1]"))





#ok was all of this craziness worth it? Let's go back to the original interaction model!
#ok bp wants to compare to the bay*period model
(em3 <- emtrends(tmb3, ~Bay, "Period"))
test(em3)
#small differences between tmb3 and tmb 5, but not much
pred_tmb3 <- ggpredict(tmb3, c("Period[15]", "Bay","Num_quads[1]"))
#about 1 oyster in AB, 0.1 oyster in Pensacola, and 3 oysters in St. Andrew. 



############################
############################

#now for seed and legal (using best model from above)
tmb5.1_seed <- glmmTMB(Sum_seed ~ Period + Bay + (Period|Site) + Period:Bay + offset(log(Num_quads)),
                  data = d5, family="nbinom2") #converge
summary(tmb5.1_seed)

(em1_seed <- emtrends(tmb5.1_seed, ~Bay, "Period"))
test(em1_seed)

pred_tmb5.1_seed <- ggpredict(tmb5.1_seed, c("Period[15]", "Bay","Num_quads[1]"))

##
tmb5.1_legal <- glmmTMB(Sum_legal ~ Period + Bay + (Period|Site) + Period:Bay + offset(log(Num_quads)),
                       data = d5, family="nbinom2",control=glmmTMBControl(optimizer=optim,
                                                                          optArgs=list(method="BFGS"))) #converge
summary(tmb5.1_legal)

(em1_legal <- emtrends(tmb5.1_legal, ~Bay, "Period"))
test(em1_legal)

pred_tmb5.1_legal <- ggpredict(tmb5.1_legal, c("Period[15]", "Bay","Num_quads[1]"))


############################
############################
############################
############################
############################
############
#now Apalachicola Bay only to examine different projects within this bay
#remember the other bays only have one project


#the original dp4 to Ben did not have NFWF-2021 period 15. 
#that's why there are 3 rows different


dp4 <- read.csv("dp4x.csv")

dp4$Site<-as.factor(dp4$Site)
dp4$Project<-as.factor(dp4$Project)


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

## dp4$SP <- paste(dp4$Site, dp4$Project, sep = "_")

## BMB: the same but 'looks' like an interaction
dp4$SP <- with(dp4, interaction(Site, Project, sep = "_", drop = TRUE))

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
##graph

ggplot(dp4, aes(Period, Sum_spat)) + facet_wrap(~Project) + geom_point() + geom_line(aes(group = SP), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())

## sum-to-zero contrasts so main effect of Period = unweighted average across Bays
options(contrasts = c("contr.sum", "contr.poly"))

##bill switches everything to bfgs


#Intercept
tmb0.AB <- glmmTMB(Sum_spat ~ (1|SP) + offset(log(Num_quads)),
                data = dp4, family="nbinom2", control=glmmTMBControl(optimizer=optim,
                                                                     optArgs=list(method="BFGS"))) #converge
summary(tmb0.AB)

#Period
tmb1.AB <- update(tmb0.AB, . ~ . + Period, control=glmmTMBControl(optimizer=optim,
                                                                  optArgs=list(method="BFGS")))
summary(tmb1.AB)

#Period + Project
tmb2.AB <- update(tmb1.AB, . ~ . + Project, control=glmmTMBControl(optimizer=optim,
                                                                   optArgs=list(method="BFGS")))
summary(tmb2.AB)

#compare optimizers

tmb2.AB1 <- update(tmb1.AB, . ~ . + Project)

#fixed effects
all.equal(fixef(tmb2.AB1), fixef(tmb2.AB))



#Period*Project
tmb3.AB <- update(tmb2.AB, . ~ . + Period:Project, control=glmmTMBControl(optimizer=optim,
                                                                          optArgs=list(method="BFGS")))
summary(tmb3.AB)

###just to compare optimizers
#default nlminb
tmb3.AB1 <- update(tmb2.AB, . ~ . + Period:Project)
#bfgs
tmb3.AB2 <- update(tmb2.AB, . ~ . + Period:Project, control=glmmTMBControl(optimizer=optim,
                                                                           optArgs=list(method="BFGS")))
#fixed effects
all.equal(fixef(tmb3.AB1), fixef(tmb3.AB2))
#yes all equal


#plot coefficients
plot_summs(tmb3.AB2)

########################

#Project
tmb4.AB <- update(tmb0.AB, . ~ . + Project, control=glmmTMBControl(optimizer=optim,
                                                                   optArgs=list(method="BFGS")))
summary(tmb4.AB)

#Ben note below
## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC

#bill note on tmb5. Ben removes the random effect term on site and then nests it
#period

#ben note below
#Period*bay/site (allow period by site across project)
tmb5.AB <- update(tmb3.AB, . ~ . - (1|SP) + (Period|SP),control=glmmTMBControl(optimizer=optim,
                                                                                 optArgs=list(method="BFGS")))
diagnose(tmb5.AB)  ##  BMB: this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5.AB)

summary(tmb5.AB)


#plot coefficients
plot_summs(tmb5.AB)


#full version of tmb5.AB written out by bp (to make sure I follow shorthand)

tmb5.1.AB <- glmmTMB(Sum_spat ~ Period + Project + (Period|SP) + Period:Project + offset(log(Num_quads)),
                  data = dp4, family="nbinom2", control=glmmTMBControl(optimizer=optim,
                                                                        optArgs=list(method="BFGS"))) 
summary(tmb5.1.AB)

tmb5.1.AB1 <- glmmTMB(Sum_spat ~ Period + Project + (Period|SP) + Period:Project + offset(log(Num_quads)),
                     data = dp4, family="nbinom2") 

summary(tmb5.1.AB1)

#fixed effects
all.equal(fixef(tmb5.1.AB1), fixef(tmb5.1.AB))
#there are some differences 

##back to Ben's code

#this is just tmb5.AB but adds a unique dispersion term for each project 

#all + dispersion
tmb6.AB <- update(tmb5.AB, dispformula = ~Project,control=glmmTMBControl(optimizer=optim,
                                                                           optArgs=list(method="BFGS")))
summary(tmb6.AB)

#bp note, tmb6.AB is tmb5.AB + adding a unique dispersion parameter for each project. 
#has singular convergence issue goes away with bfgs 

tmb7.AB <- update(tmb3.AB, . ~ .  + (0+Period|SP), dispformula = ~Project, control=glmmTMBControl(optimizer=optim,
                                                                                                  optArgs=list(method="BFGS")))
summary(tmb7.AB)

#bp note 
#Sum_spat ~ (1 | SP) + Period + Project + (0 + Period | SP) +  
#Period:Project + offset(log(Num_quads))
#and bp thinks this is the model Ben is referring to in the email 
#independent intercept + slope random effects

#model selection information

## self-naming list
cand.set2.AB =
    list(tmb0.AB,tmb1.AB,tmb2.AB,tmb3.AB,tmb4.AB, tmb5.AB, tmb6.AB, tmb7.AB)
modnames2.AB = c("intercept", "period", "period + project", "period*project", "project", "all",
              "all + dispersion", "project/sp uncorr + disp")
names(cand.set2.AB) <- modnames2.AB


#AIC
aictab(cand.set2.AB, modnames2.AB, second.ord = FALSE) #model selection table with AIC
#AICc
aictab(cand.set2.AB, modnames2.AB, second.ord = TRUE) #model selection table with AICc


#AIC(c) table of all models
AICctab(cand.set2.AB, weights=TRUE)
## best model is 'full' model, but without dispersion
#that's Ben's comment above


#plot coefficients
plot_summs(tmb5.AB, tmb6.AB)


## quantify and test trends by project
(em1.AB <- emtrends(tmb5.AB, ~Project, "Period"))
test(em1.AB)

#ok bp wants to compare to the project*period model
(em3.AB <- emtrends(tmb3.AB, ~Project, "Period"))
test(em3.AB)

#ok let's look at results of tmb6 and tmb7

(em6.AB <- emtrends(tmb6.AB, ~Project, "Period"))
test(em6.AB)

(em7.AB <- emtrends(tmb7.AB, ~Project, "Period"))
test(em7.AB)


#predict by project or period

ggpredict(tmb5.AB)
test.nfwf1 = ggpredict(tmb5.AB, terms = c("Period[9]", "Project[NFWF-1]", "Num_quads[1]"), type = c('fe')) 
test.nrda4044 = ggpredict(tmb5.AB, terms = c("Period[13]", "Project[NRDA-4044]", "Num_quads[1]"), type = c('fe')) 
test.nrda5077 = ggpredict(tmb5.AB, terms = c("Period[12]", "Project[GEBF-5007]", "Num_quads[1]"), type = c('fe')) 
test.fwc2021 = ggpredict(tmb5.AB, terms = c("Period[15]", "Project[NFWF-2021]", "Num_quads[1]"), type = c('fe')) 



#########
#check autocorrelation of best
#
#########

#check autocorrelation issues
res1 <- simulateResiduals(tmb5.AB)
plot(res1)
agg.res1 = recalculateResiduals(res1,group=dp4$Period)
time = unique(dp4$Period)
plot(time,agg.res1$scaledResiduals,pch=16)


#now tmb5 w. discharge
tmb5.12k <- update(tmb5.AB, . ~ . + Lowdays_12, control=glmmTMBControl(optimizer=optim,
                                                                       optArgs=list(method="BFGS")))
summary(tmb5.12k)

tmb5.lag12 <- update(tmb5.AB, . ~ . + lag1, control=glmmTMBControl(optimizer=optim,
                                                                   optArgs=list(method="BFGS")))
summary(tmb5.lag12)
tmb5.6k <- update(tmb5.AB, . ~ . + Lowdays_6, control=glmmTMBControl(optimizer=optim,
                                                                     optArgs=list(method="BFGS")))
tmb5.lag6 <- update(tmb5.AB, . ~ . + lag1_6, control=glmmTMBControl(optimizer=optim,
                                                                    optArgs=list(method="BFGS")))

cand.set3.1.AB =
  list(tmb5.AB,tmb5.12k,tmb5.lag12, tmb5.6k, tmb5.lag6)
modnames3.1.AB = c("full", "full_low12k", "full_12k_lag", "full_low6k", "full_6k_lag")
names(cand.set3.1.AB) <- modnames3.1.AB

#AICc
aictab(cand.set3.1.AB, modnames3.1.AB, second.ord = TRUE) #model selection table with AICc

#no improvement with the different discharge metrics

###############





library(dplyr)
library(purrr)
library(broom.mixed)

emtrends(cand.set2.AB[["period*project"]], ~Project, "Period")

cand.set.3 <- cand.set2.AB[-c(1,2,3,5)] ## cases with project * period interaction

#bp note dropping intercept, period only, project only, period+project models
#ben's note above identifies these as the project*period models

## extract delta-AIC values for all of these models
a2 <- (purrr::map_dfr(cand.set.3,
                      glance,
                      .id = "model")
    |> select(model, AIC)
    |> mutate(sing_fit = c(FALSE, FALSE, TRUE, TRUE, FALSE))
    |> mutate(across(AIC, ~ . - min(.)))
    |> arrange(desc(AIC))
)

## get emtrends results per model
res <- (purrr::map_dfr(
           cand.set.3,
           ~ emtrends(., ~Project, "Period") |> as_tibble(),
           .id = "model")
    |> full_join(a2, by = "model")
    ## sort by delta-AIC
    |> mutate(across(model, factor, levels = a2$model))
)

## plot 
ggplot(res, aes(Period.trend, Project, colour = model)) +
    geom_pointrange(aes(xmin  = lower.CL, xmax = upper.CL),
                    ## would like to do this, but messes up the order
                    ## , shape = sing_fit),
                    position = position_dodge(width = 0.2)) +
    scale_colour_brewer(palette = "Dark2", guide = guide_legend(reverse=TRUE)) +
    geom_vline(xintercept = 0, lty = 2) +
    scale_x_continuous(limits = c(-1, 1), oob = scales::squish)

res2 <- (purrr::map_dfr(
           cand.set.3,
           broom.mixed::tidy,
           effects = "ran_pars",
           conf.int = TRUE,
           .id = "model")
    |> select(model, term, estimate, lwr = conf.low, upr = conf.high)
    |> full_join(a2, by = "model")
    ## sort by delta-AIC
    |> mutate(across(model, factor, levels = a2$model))
)

ggplot(res2, aes(estimate, model)) +
    geom_pointrange(aes(xmin = lwr, xmax = upr)) +
    facet_wrap(~term, scale = "free_x") +
    expand_limits(x=0)

#bp note this conclusion below by Ben was written before I added the period 15
#data and switched to bfgs

## conclusion

## little difference in predictions for the top two models.

## singular fits are a little concerning, but doesn't make much difference
## best-AIC model is singular, next-best (delta-AIC = 5) is not, but these
## two are very similar in predictions/CIs (and presumably p-values as well,
## haven't looked)

## Omitting difference in dispersion while including interaction makes NFWF-2021 CIs ridiculous
## Additive model is bad and makes very different predictions.

## variation among SP in trend is small and/or hard to quantify
## CIs are not well computed
## almost certainly not worth worrying about very much


