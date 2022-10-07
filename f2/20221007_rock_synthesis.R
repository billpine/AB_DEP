#this is the rock synthesis but updated to just read in the data
#that are compiled in 20220327_rock_synthesis.R

d3 <- read_csv("d3_rock.csv")

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

# fails with bfgs as well
tmb5.x <- update(tmb3, . ~ . - (1|Site) + (Period|Site), control=glmmTMBControl(optimizer=optim,
                                                                                optArgs=list(method="BFGS")))



tmb6 <- update(tmb5, dispformula = ~Bay)

# fails with bfgs as well
# tmb6 <- update(tmb5, dispformula = ~Bay, control=glmmTMBControl(optimizer=optim,
#                                                                  optArgs=list(method="BFGS")))
summary(tmb6)

#drop tmb5 & 6 bc of convergence (w/ either optimizer)
cand.set2 = list(tmb0,tmb1,tmb2,tmb3,tmb4)
modnames2 = c("intercept", "period", "period + bay", "period*bay", "bay")
names(cand.set2) <- modnames2

#model selection information

aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
aictab(cand.set2, modnames2, second.ord = TRUE) #model selection table with AICc

(em1 <- emtrends(tmb5, ~Bay, "Period"))
test(em1)

ggpredict(tmb5)

pred_tmb5<- ggpredict(tmb5, c("Period[15]", "Bay","Num_quads[1]"))
#about 1 oyster in AB, 0.1 oyster in Pensacola, and 3 oysters in St. Andrew. 

# 
# test.nfwf1 = ggpredict(tmb5, terms = c("Period[9]", "Project[NFWF-1]", "Num_quads[1]"), type = c('fe')) 
# test.nrda4044 = ggpredict(tmb5, terms = c("Period[13]", "Project[NRDA-4044]", "Num_quads[1]"), type = c('fe')) 
# test.nrda5077 = ggpredict(tmb5, terms = c("Period[12]", "Project[GEBF-5007]", "Num_quads[1]"), type = c('fe')) 
# test.fwc2021 = ggpredict(tmb5, terms = c("Period[15]", "Project[NFWF-2021]", "Num_quads[1]"), type = c('fe')) 



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
tmb0.AB <- glmmTMB(Roundwt ~ (1|SP) + offset(log(Num_quads)),
                   data = dApalach, family="nbinom2") #converge
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

#Ben note below
## This is a better model, both in principle (we do want to allow for temporal trends
## to vary across sites) and in terms of AIC

#bill note on tmb5. Ben removes the random effect term on site and then nests it
#period

#ben note below
#Period*Project/site (allow period by site across Project)
tmb5.AB <- update(tmb3.AB, . ~ . - (1|SP) + (Period|SP))
diagnose(tmb5.AB)  ##  BMB: IF this is a *singular fit*: correlation of -1
## means this is probably overfitted
VarCorr(tmb5.AB)

summary(tmb5.AB)

#plot coefficients
plot_summs(tmb5.AB)

#all + dispersion
tmb6.AB <- update(tmb5.AB, dispformula = ~Project)
summary(tmb6.AB)

#bp note, tmb6 is tmb5 + adding a unique dispersion parameter for each Project. 
#has singular convergence issue goes away with bfgs 

tmb7.AB <- update(tmb3.AB, . ~ .  + (0+Period|SP), dispformula = ~Project)
summary(tmb7.AB)

diagnose(tmb7.AB)
VarCorr(tmb7.AB)

#model selection information

## self-naming list
cand.set2 =
  list(tmb0.AB, tmb1.AB, tmb2.AB, tmb3.AB, tmb4.AB, tmb5.AB, tmb6.AB, tmb7.AB)
modnames2 = c("intercept", "period", "period + Project", "period*Project", "Project", "all",
              "all + dispersion", "Project/sp uncorr + disp")
names(cand.set2) <- modnames2


#AIC
aictab(cand.set2, modnames2, second.ord = FALSE) #model selection table with AIC
#AICc
aictab(cand.set2, modnames2, second.ord = TRUE) #model selection table with AICc


(em1 <- emtrends(tmb3.AB, ~Project, "Period"))
test(em1)

#plot coefficients
plot_summs(tmb3.AB)


test.nfwf1 = ggpredict(tmb3.AB, terms = c("Period[9]", "Project[NFWF-1]", "Num_quads[1]"), type = c('fe')) 
test.nrda4044 = ggpredict(tmb3.AB, terms = c("Period[13]", "Project[NRDA-4044]", "Num_quads[1]"), type = c('fe')) 
test.nrda5077 = ggpredict(tmb3.AB, terms = c("Period[12]", "Project[GEBF-5007]", "Num_quads[1]"), type = c('fe')) 
test.fwc2021 = ggpredict(tmb3.AB, terms = c("Period[15]", "Project[FWC-2021]", "Num_quads[1]"), type = c('fe')) 
