#Data cleaning code to clean up the
#very complicated date time stamp in the DEP database
#the issue is a variety of random spaces that are separators
#between day month year time
#developed by Matt Richardson, UF
#January 2022

library(tidyverse)
library(lubridate)

rm(list=ls())

break_date.fn<-function(x){
  final_date.df<-data.frame(matrix(ncol=3,nrow=0))
  colnames(final_date.df)<-c("Month","Day","Year")
  for(i in 1:length(x)){
    #Uses just the month, day, and year columns. If you want          
    #the time just remove the [1:3] index.
    x<-data$Harvested
    a<-unlist(strsplit(strsplit(x[i],split=" ")[[1]][1],split="/"))
    names(a)<-c("Month","Day","Year")
    b<-t(as.data.frame(a))
    final_date.df<-rbind(final_date.df,b)
  }
  rownames(final_date.df)<-1:length(x)
  return(final_date.df)
}

break_date2.fn<-function(x){
  final_date.df<-data.frame(matrix(ncol=3,nrow=0))
  colnames(final_date.df)<-c("Month","Day","Year")
  for(i in 1:length(x)){
    #Uses just the month, day, and year columns. If you want          
    #the time just remove the [1:3] index.
    a<-strsplit(x[i],split=" ")[[1]]
    char_length<-c()
    for(j in 1:length(a)){
      char_length<-c(char_length,nchar(a[j]))
    }
    a<-a[-grep(char_length,pattern="0")]
    a<-a[1:3]
    names(a)<-c("Month","Day","Year")
    b<-t(as.data.frame(a))
    final_date.df<-rbind(final_date.df,b)
  }
  rownames(final_date.df)<-1:length(x)
  return(final_date.df)
}

data<-read.csv("~/GitHub/AB_DEP/Data_4044A_Final.csv")
colnames(data)[1]<-"Harvested"

data[,21:23]<-break_date2.fn(data$Harvested)
colnames(data)[21:23]<-c("Month","Day","Year")
data$full_year<-paste(data$Month,data$Day,data$Year,sep="/")
data$full_year<-mdy(data$full_year)
plot(data$Weight_kg~data$full_year)

d2 <- data %>%
  mutate(Year = year(data$full_year),
         Month = month(data$full_year),
         Day = day(data$full_year))




write.csv(data,"~/GitHub/AB_DEP/Data_5007A_Final_cleaned.csv",row.names = F)
