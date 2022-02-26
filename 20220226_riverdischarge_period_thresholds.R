##Organizing River Discharge Data
#organize by period (winter/summer by year)
#pull out number of days above/below threshold within a period

library(waterData)

#river discharge data - apalachicola
#station to analyze
station = '02358000'   
#get site name
stinfo  = siteInfo(station)
#read entire time series
dis = importDVs(staid=station,code='00060',stat='00003', sdate= "2015-01-01") 
dis$month = as.numeric(strftime(dis$dates,format="%m"))
dis$year = as.numeric(strftime(dis$dates,format="%Y"))
#column names
colnames(dis) <- c("StaID", "Discharge", "Date", "QualCode", "Month", "Year")

#attach period to the data
dis$Period <- NA
firstyear <- min(dis$Year)
endyear <- max(dis$Year)
years <- sort(rep(firstyear:endyear, times = 1, each = 2))
for(i in unique(years)){
  y <- i #year
  p <- which(years == i) #period number - 2010 = 1 and 2, 2011 = 3 and 4, and so forth.
  for(j in 1:nrow(dis)){
    if(dis$Year[j] == y & dis$Month[j] > 3 & dis$Month[j] < 10) dis$Period[j] = p[1] #year i months 4-9
    if(dis$Year[j] == y & dis$Month[j] > 9) dis$Period[j] = p[2] #year i months 10-12
    if(dis$Year[j] == y+1 & dis$Month[j] < 4) dis$Period[j] = p[2] #year i+1 months 1-3
  }
}

#low threshold 12000 cfs is level to inundate floodplain
below <- function(x){
  length(x[x<12000])
}

#high threshold above 12000 inundation of floodplain
above <- function(x){
  length(x[x>12000])
}


#gives us a table of the number of days within the period where discharge falls above or below the set threshold
belowThreshold <- aggregate(Discharge ~ Period, data = dis, FUN = below)
aboveThreshold <- aggregate(Discharge ~ Period, data = dis, FUN = above)

belowThreshold
aboveThreshold
