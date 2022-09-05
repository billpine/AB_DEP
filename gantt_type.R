#https://www.statology.org/gantt-chart-r-ggplot2/
#create data frame
data <- data.frame(name = c('AB NRDA-4044', 'AB GEBF-5007', 'AB NFWF-1', 'AB NFWF-2021'), 
                   start = c(2, 6, 2, 13),
                   end = c(15, 15, 15, 15),
                   action = c('in-water'))

p1<-ggplot(data, aes(x=start, xend=end, y=name, yend=name, color=action)) +
  theme_bw()+ #black grid and white background
  geom_segment(size=3) + #line width
  labs(title='Apalachicola Bay', x='Time', y='Project')+
  scale_x_continuous(breaks=seq(2,15,1))

##now overlap the points below for each project on top of the segment
##so you can see when sampling took place


#make geo points or short segments for the sampling of each project
data1 <- data.frame(name = c('AB NRDA-4044'), 
                   start = c(3, 5, 6, 7, 9, 10, 12, 13),
                   end = c(3, 5, 6, 7, 9, 10, 12, 13),
                   action = c('Monitoring'))

data2 <- data.frame(name = c('AB GEBF-5007'), 
                    start = c(7,9,11,12),
                    end = c(7,9,11,12),
                    action = c('Monitoring'))

data3 <- data.frame(name = c('AB NFWF-1'), 
                    start = c(2,3,4,5,6,7,8,9),
                    end = c(2,3,4,5,6,7,8,9),
                    action = c('Monitoring'))

data4 <- data.frame(name = c('AB NFWF-2021'), 
                    start = c(13,14,15),
                    end = c(13,14,15),
                    action = c('Monitoring'))


names(data)
names(data1)

z<-rbind(data,data1,data2,data3,data4)

names(z)
unique(z$action)


ggplot(z, aes(x=start, xend=end, y=name, yend=name, color=action)) +
  geom_segment(size=0.5)+
  scale_x_continuous(breaks=seq(2,15,1))+
  geom_point(size=5,aes(x=start,y=name, color=action))+#here is the problem I only want geom_point for action = "monitoring"
  theme_bw()+ #black grid and white background
  labs(title='Apalachicola Bay', x='Time', y='Project')
  


# 
# unique(z$action)
# 
# p1+geom_segment(data1,aes(x=start, xend=end, y=name, yend=name, color=action))
# 
# p1+geom_point()
# 
# 
# test<-ggplot()+
# geom_point(data1[data1$name=="NFWF-1"],aes(x=start, xend=end, y=name, yend=name, color=action))