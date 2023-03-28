#https://www.statology.org/gantt-chart-r-ggplot2/
#create data frame

library(ggplot2)

data <- data.frame(name = c('AB NRDA-4044', 'AB GEBF-5007', 'AB NFWF-1', 'AB NFWF-2021'), 
                   start = c(2, 6, 2, 13),
                   end = c(15, 15, 15, 15),
                   action = c('in-water'))

p1<-ggplot(data, aes(x=start, xend=end, y=name, yend=name, color=action)) +
  theme_bw()+ #black grid and white background
  geom_segment(size=3) + #line width
  labs(title='Apalachicola Bay', x='Period', y='Project')+
  scale_x_continuous(breaks=seq(2,15,1))

##now overlap the points below for each project on top of the segment
##so you can see when sampling took place


#make geo points or short segments for the sampling of each project
data1 <- data.frame(name = c('AB NRDA-4044'), 
                   start = c(3, 5, 6, 7, 9, 10, 12, 13, 14),
                   end = c(3, 5, 6, 7, 9, 10, 12, 13, 14),
                   action = c('Monitoring'))

data2 <- data.frame(name = c('AB GEBF-5007'), 
                    start = c(7,9,12,13),
                    end = c(7,9,12,13),
                    action = c('Monitoring'))

data3 <- data.frame(name = c('AB NFWF-1'), 
                    start = c(2,3,4,5,6,7,8,9),
                    end = c(2,3,4,5,6,7,8,9),
                    action = c('Monitoring'))

data4 <- data.frame(name = c('AB NFWF-2021'), 
                    start = c(13,14,15),
                    end = c(13,14,15),
                    action = c('Monitoring'))


# names(data)
# names(data1)
# 
# z<-rbind(data,data1,data2,data3,data4)
# 
# names(z)
# unique(z$action)
# 
# 
# ggplot(z, aes(x=start, xend=end, y=name, yend=name, color=action)) +
#   geom_segment(size=2)+
#   scale_x_continuous(breaks=seq(2,15,1))+
#   geom_point(size=5,aes(x=start,y=name, color=action))+#here is the problem I only want geom_point for action = "monitoring"
#   theme_bw()+ #black grid and white background
#   labs(title='Apalachicola Bay', x='Time', y='Project')
#   
# 
# 
# ####from Matt
# z_new<-rbind(data1,data2,data3,data4) 
# 
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# colors<-gg_color_hue(2)
# 
# ggplot() +
#   
#   geom_segment(data=data,aes(x=start, xend=end, y=name, yend=name),size=1.5,color=colors[1])+
#   
#   scale_x_continuous(breaks=seq(2,15,1))+
#   
#   geom_point(data=z_new,aes(x=start,y=name, color=action),size=5)+#here is the problem I only want geom_point for action = "monitoring"
#   
#   theme_bw()+ #black grid and white background
#   
#   labs(title='Apalachicola Bay', x='period', y='Project',color="Action")+
#   
#   theme(plot.title = element_text(hjust=0.5))+
#   
#   scale_color_manual(values=colors[2]) 
# 
# ggsave("AB_gantt_type.png", width = 10, height = 10)

#from Matt 2

z_new<-rbind(data1,data2,data3,data4) 

z_new$Sampling_Size<-"Normal"
z_new$Sampling_Size[1]<-"Abnormal"

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors<-gg_color_hue(2)

p1<-ggplot() +
  
  geom_segment(data=data,aes(x=start, xend=end, y=name, yend=name),size=0.5,color=colors[1])+
  
  scale_x_continuous(breaks=seq(2,15,1))+
  
  geom_point(data=z_new,aes(x=start,y=name, color=action,shape=Sampling_Size),size=5)+
  
  theme_bw()+
  
  labs(title='Apalachicola Bay', x='Period', y='Project',color="Action")+
  
  theme(plot.title = element_text(hjust=0.5))+
  
  scale_color_manual(values=colors[2])+
  
  scale_shape_manual(guide="none",values=c(1,16))


#period 2 is 2015
#period 3 is 2016
#period 4 is 
#period 15 is 2022

p2<- p1 +
  

  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  coord_cartesian(clip = "off") +
  annotate(geom = "text",
           x = seq(from = 2, to = 15, by = 1),
           y = 0,
           label = seq(from = 2, to = 15, by = 1),
           vjust = 3.5) +
  annotate(geom = "text",
           x = seq(from = 2, to = 15, by = 1),
           y = 0,
           label = seq(from = 2015, to = 2022, by = 1),
           vjust = 5)
p2


ggsave("AB_gantt_type.png", width = 10, height = 10)


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