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

d5 <- read.csv("d5.csv")

d5$Bay <- as.factor(d5$Bay)

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

#all + dispersion
tmb6 <- update(tmb5, dispformula = ~Bay)
summary(tmb6)
ggplot(d5, aes(Period, Sum_spat)) + facet_wrap(~Bay) + geom_point() + geom_line(aes(group = Site), alpha = 0.5) +
  scale_y_continuous(trans = scales::log1p_trans())

cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4, tmb5, tmb6)
modnames2 = c("intercept", "period", "period + bay", "period*bay", "bay", "period*bay/site",
              "all + dispersion")
names(cand.set2) <- modnames2

#AIC(c) table of all models
AICctab(cand.set2, weights=TRUE)
## best model is 'full' model, but without dispersion
## This is Ben's comment above, what he means is best is
## the best is tmb5, which is same as tmb6, but tmb6 includes
## unique dispersion parameter for each Bay


## quantify and test trends by bay
#em notes https://bookdown.org/dereksonderegger/571/4-contrasts.html
#https://cran.r-project.org/web/packages/emmeans/emmeans.pdf

#report this info below in paper.
#these are the beta values for each bay, not marginal means

(em1 <- emtrends(tmb5, ~Bay, "Period"))
test(em1)
##need to compare this to summary(tmb5) to check betas. st andrew weird
#> -0.05967+0.05610
#[1] -0.00357

#> -0.05967+-0.32938
#[1] -0.38905


#marginal means for tmb5
mm.tmb5<-emmeans(tmb5, ~Bay, "Period")


#ok bp wants to compare to the bay*period model
(em3 <- emtrends(tmb3, ~Bay, "Period"))
test(em3)
#small differences, but not much


aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC

############################
############################

#now for seed and legal
tmb5.1_seed <- glmmTMB(Sum_seed ~ Period + Bay + (Period|Site) + Period:Bay + offset(log(Num_quads)),
                  data = d5, family="nbinom2") #converge
summary(tmb5.1_seed)



############################
############################

############
#now Apalachicola Bay only to examine different projects within this bay
#remember the other bays only have one project

dp4 <- read.csv("dp4.csv")

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
diagnose(tmb5.AB)  ##  BMB: this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5.AB)



summary(tmb5.AB)

#full version of tmb5.AB written out by bp (to make sure I follow shorthand)

tmb5.1.AB <- glmmTMB(Sum_spat ~ Period + Project + (Period|SP) + Period:Project + offset(log(Num_quads)),
                  data = dp4, family="nbinom2") 
summary(tmb5.1.AB)


##back to Ben's code

#all + dispersion
tmb6.AB <- update(tmb5.AB, dispformula = ~Project)
summary(tmb6.AB)

#bp note, this is tmb5.AB + adding a unique dispersion parameter for each project. 
#still doesn't converge, but based on emails with Ben will go with this one plus tmb7.ab below
#that does converge

tmb7.AB <- update(tmb3.AB, . ~ .  + (0+Period|SP), dispformula = ~Project)
summary(tmb7.AB)

#bp note 
#Sum_spat ~ (1 | SP) + Period + Project + (0 + Period | SP) +  
#Period:Project + offset(log(Num_quads))
#and bp thinks this is the model Ben is referring to in the email 
#independent intercept + slope random effects

ggplot(dp4, aes(Period, Sum_spat)) + facet_wrap(~Project) + geom_point() + geom_line(aes(group = SP), alpha = 0.5) +
    scale_y_continuous(trans = scales::log1p_trans())


## self-naming list
cand.set2.AB =
    list(tmb0.AB,tmb1.AB,tmb2.AB,tmb3.AB,tmb4.AB, tmb5.AB, tmb6.AB, tmb7.AB)
modnames2.AB = c("intercept", "period", "period + project", "period*project", "project", "period*project/sp",
              "all + dispersion", "project/sp uncorr + disp")
names(cand.set2.AB) <- modnames2.AB

#AIC(c) table of all models
AICctab(cand.set2.AB, weights=TRUE)
## best model is 'full' model, but without dispersion

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


aictab(cand.set2.AB, modnames2.AB, second.ord = FALSE) #model selection table with AIC

library(dplyr)
library(purrr)
library(broom.mixed)

emtrends(cand.set2.AB[["period*project"]], ~Project, "Period")

cand.set.3 <- cand.set2.AB[-c(1,2,5)] ## cases with project * period interaction

#bp note dropping intercept, period only, and project only models
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


