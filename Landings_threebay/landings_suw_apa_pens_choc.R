### Conversions for oyster landings data ###
# Goal - to get the volume of shell removed each year based off of the landings data
# to do this we must somehow convert the meat weight provided in the landings data to the volume of shell
# we can do this by
# meat weight --> # of bushels --> # of oysters --> meat weight per oyster --> pounds to grams --> dry weight --> height --> volume per oyster --> total volume

## Harvest Areas of interest and the counties used to represent them ##
#Suwannee = Dixie and Levy
#Apalachicola = Gulf and Franklin
#Pensacola = Santa Rosa and Escambia
#Choctawhatchee/St. Andrew = Ocaloosa and Walton


setwd("~/Git/AB_DEP/Landings_threebay")

library(dplyr)
library(ggplot2)
library(ggpubr)

## reading in the landings data and cleaning it up to include only the counties we are interested in ##

post_1984 <- read.csv("oyster_landings_by_county_84to22.csv", header = TRUE)

pst2<- subset(post_1984, post_1984$Year > 1985)

post_1984 <- pst2

#post_1984 <- select(post_1984, Year, County_Landed, Pounds)

#Suwannee
Dixie <- filter(post_1984, County_Landed == "DIXIE")
Levy <- filter(post_1984, County_Landed == "LEVY")
Dixie <- select(Dixie, Year, Pounds, Trips, Average_Price, Estimated_Value)
Levy <- select(Levy, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Dixie) <- c("Year", "D.pounds","D.trips","D.avg_price","D.est.val") 
colnames(Levy) <- c("Year", "L.pounds","L.trips","L.avg_price","L.est.val") 
suw_counties <- full_join(Dixie, Levy, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Dixie1<-data.frame(Year=suw_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                   pounds=suw_counties$D.pounds, 
                   trips=suw_counties$D.trips, 
                   avg_price=suw_counties$D.avg_price, 
                   est_val=suw_counties$D.est.val)
Levy1<-data.frame(Year=suw_counties$Year, 
                  pounds=suw_counties$L.pounds, 
                  trips=suw_counties$L.trips, 
                  avg_price=suw_counties$L.avg_price, 
                  est_val=suw_counties$L.est.val)
Levy1[is.na(Levy1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Dixie1[is.na(Dixie1)] <- 0 # replace NAs with 0 so we can add columns in the next step
suw<-Dixie1+Levy1 # add Dixie and Levy data to consolidate into suwannee
suw$Year<-suw$Year/2 # fix the year

#Apalachicola
Gulf <- filter(post_1984, County_Landed == "GULF")
Franklin <- filter(post_1984, County_Landed == "FRANKLIN")
Gulf <- select(Gulf, Year, Pounds, Trips, Average_Price, Estimated_Value)
Franklin <- select(Franklin, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Gulf) <- c("Year", "G.pounds","G.trips","G.avg_price","G.est.val") 
colnames(Franklin) <- c("Year", "F.pounds","F.trips","F.avg_price","F.est.val") 
apa_counties <- full_join(Gulf, Franklin, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Gulf1<-data.frame(Year=apa_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                   pounds=apa_counties$G.pounds, 
                   trips=apa_counties$G.trips, 
                   avg_price=apa_counties$G.avg_price, 
                   est_val=apa_counties$G.est.val)
Franklin1<-data.frame(Year=apa_counties$Year, 
                  pounds=apa_counties$F.pounds, 
                  trips=apa_counties$F.trips, 
                  avg_price=apa_counties$F.avg_price, 
                  est_val=apa_counties$F.est.val)
Franklin1[is.na(Franklin1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Gulf1[is.na(Gulf1)] <- 0 # replace NAs with 0 so we can add columns in the next step
apa<-Gulf1+Franklin1 # add Gulf and Franklin data to consolidate into apalachicola
apa$Year<-apa$Year/2 # fix the year

#Pensacola
Santa_Rosa <- filter(post_1984, County_Landed == "SANTA ROSA")
Escambia <- filter(post_1984, County_Landed == "ESCAMBIA")
Santa_Rosa <- select(Santa_Rosa, Year, Pounds, Trips, Average_Price, Estimated_Value)
Escambia <- select(Escambia, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Santa_Rosa) <- c("Year", "SR.pounds","SR.trips","SR.avg_price","SR.est.val") 
colnames(Escambia) <- c("Year", "E.pounds","E.trips","E.avg_price","E.est.val") 
pens_counties <- full_join(Santa_Rosa, Escambia, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Santa_Rosa1<-data.frame(Year=pens_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                  pounds=pens_counties$SR.pounds, 
                  trips=pens_counties$SR.trips, 
                  avg_price=pens_counties$SR.avg_price, 
                  est_val=pens_counties$SR.est.val)
Escambia1<-data.frame(Year=pens_counties$Year, 
                      pounds=pens_counties$E.pounds, 
                      trips=pens_counties$E.trips, 
                      avg_price=pens_counties$E.avg_price, 
                      est_val=pens_counties$E.est.val)
Escambia1[is.na(Escambia1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Santa_Rosa1[is.na(Santa_Rosa1)] <- 0 # replace NAs with 0 so we can add columns in the next step
pens<-Santa_Rosa1+Escambia1 # add Santa_Rosa and Escambia data to consolidate into pensacola
pens$Year<-pens$Year/2 # fix the year

#Choctawhatchee/St. Andrew
Okaloosa <- filter(post_1984, County_Landed == "OKALOOSA")
Walton <- filter(post_1984, County_Landed == "WALTON")
Okaloosa <- select(Okaloosa, Year, Pounds, Trips, Average_Price, Estimated_Value)
Walton <- select(Walton, Year, Pounds, Trips, Average_Price, Estimated_Value)
colnames(Okaloosa) <- c("Year", "O.pounds","O.trips","O.avg_price","O.est.val") 
colnames(Walton) <- c("Year", "W.pounds","W.trips","W.avg_price","W.est.val") 
choc_counties <- full_join(Okaloosa, Walton, by = "Year")  # join the two together into one data frame so all the years are listed for each county so we can add them together later
Okaloosa1<-data.frame(Year=choc_counties$Year,   # separate the counties out again into their own data frames so we can add them together
                  pounds=choc_counties$O.pounds, 
                  trips=choc_counties$O.trips, 
                  avg_price=choc_counties$O.avg_price, 
                  est_val=choc_counties$O.est.val)
Walton1<-data.frame(Year=choc_counties$Year, 
                      pounds=choc_counties$W.pounds, 
                      trips=choc_counties$W.trips, 
                      avg_price=choc_counties$W.avg_price, 
                      est_val=choc_counties$W.est.val)
Walton1[is.na(Walton1)] <- 0 # replace NAs with 0 so we can add columns in the next step
Okaloosa1[is.na(Okaloosa1)] <- 0 # replace NAs with 0 so we can add columns in the next step
choc<-Okaloosa1+Walton1 # add Okaloosa and Walton data to consolidate into choctawhatchee
choc$Year<-choc$Year/2 # fix the year



suw <- select(suw, Year, pounds, trips, avg_price, est_val)
apa <- select(apa, Year, pounds, trips, avg_price, est_val)
pens <- select(pens, Year, pounds, trips, avg_price, est_val)
choc <- select(choc, Year, pounds, trips, avg_price, est_val)


colnames(suw) <- c("Year", "suw.pounds","suw.trips","suw.avg_price","suw.est.val") 
colnames(apa) <- c("Year", "apa.pounds","apa.trips","apa.avg_price","apa.est.val") 
colnames(pens) <- c("Year", "pens.pounds","pens.trips","pens.avg_price","pens.est.val") 
colnames(choc) <- c("Year", "choc.pounds","choc.trips","choc.avg_price","choc.est.val") 


landings_84to22 <- full_join(suw, apa, by = "Year")
landings_84to22 <- full_join(landings_84to22, pens, by="Year")
landings_84to22 <- full_join(landings_84to22, choc, by="Year") # this is the cleaned landings data we will use

## Now we start the conversions

# converting the meat weight from the landings data to the number of bushels
landings_84to22$suw.bushels<-landings_84to22$suw.pounds/6.5625  #from FWC conversion factors
landings_84to22$apa.bushels<-landings_84to22$apa.pounds/6.5625
landings_84to22$pens.bushels<-landings_84to22$pens.pounds/6.5625
landings_84to22$choc.bushels<-landings_84to22$choc.pounds/6.5625

#converting the # of bushels to the # of oysters
landings_84to22$suw.count<-landings_84to22$suw.bushels*225  #from Berrigan paper
landings_84to22$apa.count<-landings_84to22$apa.bushels*225
landings_84to22$pens.count<-landings_84to22$pens.bushels*225
landings_84to22$choc.count<-landings_84to22$choc.bushels*225

#converting from # of oysters to meat weight per oyster
landings_84to22$suw.ind_wt<-landings_84to22$suw.pounds/landings_84to22$suw.count
landings_84to22$apa.ind_wt<-landings_84to22$apa.pounds/landings_84to22$apa.count
landings_84to22$pens.ind_wt<-landings_84to22$pens.pounds/landings_84to22$pens.count
landings_84to22$choc.ind_wt<-landings_84to22$choc.pounds/landings_84to22$choc.count

#convert from pounds to grams
landings_84to22$suw.grams<-landings_84to22$suw.ind_wt*453.592
landings_84to22$apa.grams<-landings_84to22$apa.ind_wt*453.592
landings_84to22$pens.grams<-landings_84to22$pens.ind_wt*453.592
landings_84to22$choc.grams<-landings_84to22$choc.ind_wt*453.592

#for later use
land<-landings_84to22


#### using the minimum harvest size height (75mm)

#converting from height to volume
land$logvol_suw_2<-75*0.034867+0.928140 #from Hajovsky et al
land$vol_suw_2<-exp(land$logvol_suw_2)
land$logvol_apa_2<-75*0.034867+0.928140
land$vol_apa_2<-exp(land$logvol_apa_2)
land$logvol_pens_2<-75*0.034867+0.928140
land$vol_pens_2<-exp(land$logvol_pens_2)
land$logvol_choc_2<-75*0.034867+0.928140
land$vol_choc_2<-exp(land$logvol_choc_2)

#convert from individual oyster volume to total volume per county per year
land$tot_vol_S_2<-land$vol_suw_2*land$suw.count
land$tot_vol_A_2<-land$vol_apa_2*land$apa.count
land$tot_vol_P_2<-land$vol_pens_2*land$pens.count
land$tot_vol_C_2<-land$vol_choc_2*land$choc.count

#convert from mL to meters cubed
land$Vol_S_m3_2<-land$tot_vol_S_2*0.000001
land$Vol_A_m3_2<-land$tot_vol_A_2*0.000001
land$Vol_P_m3_2<-land$tot_vol_P_2*0.000001
land$Vol_C_m3_2<-land$tot_vol_C_2*0.000001

#calculating the number of lone cabbages
LC<-16000/1.308 #yards to meters
land[is.na(land)] = 0
land$num_LC_suw<-land$Vol_S_m3_2/LC
land$num_LC_apa<-land$Vol_A_m3_2/LC
land$num_LC_pens<-land$Vol_P_m3_2/LC
land$num_LC_choc<-land$Vol_C_m3_2/LC

# Calculate CPUE, this is catch/trips
land$suw.CPUE<-land$suw.pounds/land$suw.trips
land$apa.CPUE<-land$apa.pounds/land$apa.trips
land$pens.CPUE<-land$pens.pounds/land$pens.trips
land$choc.CPUE<-land$choc.pounds/land$choc.trips


#plots

names(land)



d1 <- land %>% 
  dplyr::select(Year, suw.pounds,suw.trips,suw.avg_price,suw.CPUE,Vol_S_m3_2)

names(d1) <- c("Year", "suw.pounds","suw.trips","suw.price","suw.cpue","suw.removed" )
  
names(d1)


#### Plots for Suwannee Sound ####
#number of cubic meters removed from suwannee sound
s1<-ggplot(data = d1, aes(x = Year, y = suw.removed)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,1400),breaks=c(0,200,400,600, 800, 1000, 1200, 1400))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.title =element_text(size=10, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed")


#number of trips in suwannee sound
s2<-ggplot(data = d1, aes(x = Year, y = suw.trips)) +
  
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,12000),breaks=c(seq(0,12000, by= 1000))) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Commercial Trips",
       y = "# of Trips")

# CPUE for Suwannee Sound
s3<-ggplot(data=d1, aes(x=Year, y=suw.cpue))+
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,1995,2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,400),breaks=c(0,50,100,150,200,250,300,350,400))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Catch Per Unit Effort",
       y = "CPUE")

#landings in Suwannee sound
s4<-ggplot(data = d1, aes(x = Year, y = suw.pounds/1000)) +
    geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
    theme_classic()+
    scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1990, 1995, 2000, 2005, 
                                                    2010,2015,2020,2025)) +
    scale_y_continuous(limits=c(0,1200),breaks=c(0, 100, 200, 300, 400, 500, 600, 700, 
                                                   800, 900, 1000, 1100, 1200))+
    theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                  face="bold"),
          plot.title =element_text(size=10, face='bold', hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                      linetype="solid")) +
    labs(title = "Pounds landed",
         y = "Pounds x 1000")

suw_fig<-ggarrange(s2,s3,s4, 
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
annotate_figure(suw_fig,
                top = text_grob("Suwannee Sound", color = "black", face = "bold", size = 20))

#ggsave("suwannee_dependent.pdf", width = 10, height = 10)

#### Apalachicola ####
d2 <- land %>% 
  dplyr::select(Year, apa.pounds,apa.trips,apa.avg_price,apa.CPUE,Vol_A_m3_2)

names(d2) <- c("Year", "apa.pounds","apa.trips","apa.price","apa.cpue","apa.removed")

names(d2)

#### Plots for Apalachicola ####
#number of cubic meters removed from Apalachicola sound
a1<-ggplot(data = d2, aes(x = Year, y = apa.removed)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1985,1990,1995,2000,2005,2010,2015,2020,2025))+
  scale_y_continuous(limits=c(0,1400),breaks=c(0,200,400,600,800,1000,1200,1400))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.title =element_text(size=10, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed")


#number of trips in apalachicola
a2<-ggplot(data = d2, aes(x = Year, y = apa.trips)) +
  
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,60000),breaks=c(0,10000,20000,30000,40000,50000, 60000)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Apalachicola Commercial Trips",
       y = "# of Trips")

# CPUE for Apalachicola
a3<-ggplot(data=d2, aes(x=Year, y=apa.cpue))+
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,1995,2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,200),breaks=c(0,50,100,150,200))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Apalachicola CPUE",
       y = "CPUE")

#landings in Apalachicola
a4<-ggplot(data = d2, aes(x = Year, y = apa.pounds/1000)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1990, 1995, 2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,4000),breaks=c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 
                                               4000))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Apalachicola Landings (lbs)",
       y = "Pounds x 1000")

apa_fig<-ggarrange(a2,a3,a4, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2)
annotate_figure(apa_fig,
                top = text_grob("Apalachicola", color = "black", face = "bold", size = 20))

ggsave("apalach_dependent.pdf", width = 10, height = 10)


#### Pensacola ####
#volume in meters cubed of shell material removed from Pensacola
d3 <- land %>% 
  dplyr::select(Year, pens.pounds,pens.trips,pens.avg_price,pens.CPUE,Vol_A_m3_2)

names(d3) <- c("Year", "pen.pounds","pen.trips","pen.price","pen.cpue","pen.removed")

#### Plots for pensacola ####
#number of cubic meters removed from pensacola sound
p1<-ggplot(data = d3, aes(x = Year, y = pen.removed)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1985,1990,1995,2000,2005,2010,2015,2020,2025))+
  scale_y_continuous(limits=c(0,4000),breaks=c(seq(0,4000, by= 500)))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.title =element_text(size=10, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed")

#number of trips in pensacola
p2<-ggplot(data = d3, aes(x = Year, y = pen.trips)) +
  
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,2000),breaks=c(seq(0,2000, by= 500))) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Pensacola Commercial Trips",
       y = "# of Trips")

# CPUE for pensacola
p3<-ggplot(data=d3, aes(x=Year, y=pen.cpue))+
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,1995,2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,600),breaks=c(seq(0,600, by= 100)))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Pensacola CPUE",
       y = "CPUE")

#landings in pensacola
p4<-ggplot(data = d3, aes(x = Year, y = pen.pounds/1000)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1990, 1995, 2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,600),breaks=c(seq(0,600, by= 100)))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Pensacola landings (lbs)",
       y = "Pounds x 1000")

pen_fig<-ggarrange(p2,p3,p4, 
                   labels = c("A", "B", "C", "D"),
                   ncol = 2, nrow = 2)
annotate_figure(pen_fig,
                top = text_grob("Pensacola", color = "black", face = "bold", size = 20))

ggsave("pensacola_dependent.pdf", width = 10, height = 10)


#### Choctawhatchee ####
d4 <- land %>% 
  dplyr::select(Year, choc.pounds,choc.trips,choc.avg_price,choc.CPUE,Vol_A_m3_2)

names(d4) <- c("Year", "choc.pounds","choc.trips","choc.price","choc.cpue","choc.removed")

#### Plots for St. Andrew ####
#number of cubic meters removed from St. Andrew sound
s1<-ggplot(data = d4, aes(x = Year, y = choc.removed)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1985,1990,1995,2000,2005,2010,2015,2020,2025))+
  scale_y_continuous(limits=c(0,4000),breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        plot.title =element_text(size=10, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "Shell Material Removed",
       y = "Cubic Meters Removed")

#number of trips in St. Andrew
s2<-ggplot(data = d4, aes(x = Year, y = choc.trips)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,
                                                  1995,2000, 2005, 2010,2015,2020, 2025)) +
  scale_y_continuous(limits=c(0,200),breaks=c(0,50,100,150,200)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "St. Andrew Commercial Trips",
       y = "# of Trips")

# CPUE for St. Andrew
s3<-ggplot(data=d4, aes(x=Year, y=choc.cpue))+
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985, 1985, 1990,1995,2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,200),breaks=c(0,50,100,150,200,250))+
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                face="bold"),
        plot.title =element_text(size=10, face='bold', hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                    linetype="solid")) +
  labs(title = "St. Andrew CPUE",
       y = "CPUE")

#landings in St. Andrew
s4<-ggplot(data = d4, aes(x = Year, y = choc.pounds/1000)) +
  geom_point(colour ="red", size=3) +   geom_line(colour ="red")+
  geom_line(colour ="red")+
  theme_classic()+
  scale_x_continuous(limits=c(1985,2025),breaks=c(1985,1990, 1995, 2000, 2005, 
                                                  2010,2015,2020,2025)) +
  scale_y_continuous(limits=c(0,12),breaks=c(0, 2, 4, 6, 8, 10, 12))+
                       theme(axis.text=element_text(size=10),axis.title=element_text(size=10,
                                                                                     face="bold"),
                             plot.title =element_text(size=10, face='bold', hjust = 0.5),
                             axis.text.x = element_text(angle = 45, hjust = 1),
                             panel.border = element_rect(color = "black", size = 1, fill = NA, 
                                                         linetype="solid")) +
labs(title = "St. Andrew Landings (lbs)",
y = "Pounds x 1000")
                     
choc_fig<-ggarrange(s2,s3,s4, 
       labels = c("A", "B", "C", "D"),
       ncol = 2, nrow = 2)
   
annotate_figure(choc_fig,
                top = text_grob("St. Andrew", color = "black", face = "bold", size = 20))

ggsave("stAndrew_dependent.pdf", width = 10, height = 10)

######
#all arrange
all_fig<-ggarrange(a2,a3,a4,p2,p3,p4,s2,s3,s4, 
                    labels = c("A", "B", "C", "D", "E",
                               "F","G","H","I"),
                    ncol = 3, nrow = 3)

annotate_figure(all_fig,
                top = text_grob("Fisheries Dependent", color = "black", face = "bold", size = 20))

ggsave("all_dependent.png", width = 10, height = 10)

