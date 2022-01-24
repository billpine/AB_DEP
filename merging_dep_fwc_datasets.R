#now we will start merging the data sets
#first the two DEP
#then the FWC

#start with 4044

d1 <- read.csv("~/GitHub/AB_DEP/4044_to_merge.csv")

names(d1)


#some renaming so two DEP datasets match name columns
d1.1 <- dplyr::rename(d1,Date=Harvested, Weight=Weight_kg, Legal=Adults_75mm, Sublegal= Seed_26_74mm, Spat=Spat_0_25mm)

#subset the columns to the ones you want to work with
d1.2 <- d1.1 %>% 
  dplyr::select(Site, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, season)


#now 5077

d2 <- read.csv("~/GitHub/AB_DEP/5007_to_merge.csv")

d2.1 <- d2 %>% 
  dplyr::select(Site, Weight, Legal, Sublegal, Spat, Year, Month, Day, Period, season)

names(d2.1)


#merge live count total data frame with the tran_length total data frame

d3<-rbind(d1.2, d2.1)
