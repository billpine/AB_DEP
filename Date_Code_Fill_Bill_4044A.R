rm(list=ls())

library(lubridate)

break_date.fn<-function(x){
  final_date.df<-data.frame(matrix(ncol=3,nrow=0))
  colnames(final_date.df)<-c("Month","Day","Year")
  for(i in 1:length(x)){
    #Uses just the month, day, and year columns. If you want          
    #the time just remove the [1:3] index.
    a<-unlist(strsplit(strsplit(trimws(x[i]),split=" ")[[1]][1],split="/"))
    names(a)<-c("Month","Day","Year")
    b<-t(as.data.frame(a))
    final_date.df<-rbind(final_date.df,b)
  }
  rownames(final_date.df)<-1:length(x)
  return(final_date.df)
}




data<-read.csv("~/GitHub/AB_DEP/Data_4044A_Final.csv")
colnames(data)[1]<-"Harvested"

data[,21:23]<-break_date.fn(data$Harvested)
colnames(data)[21:23]<-c("Month","Day","Year")

data$full_year<-paste(data$Month,data$Day,data$Year,sep="-")
data$full_year<-mdy(data$full_year)

write.csv(data,"~/GitHub/AB_DEP/Data_4044A_Final_clean.csv",row.names = F)
