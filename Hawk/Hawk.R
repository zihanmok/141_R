data<-read.csv('the_hawks.csv')
library(ggplot2)
library(ggmap)
library(geosphere)
library(lubridate)
#Q1
data$tag<-as.factor(data$tag)
levels(data$tag) #check for different tag
loc<-subset(data[,c(3,4)])
map<-get_map(loc,maptype = 'toner',zoom=9,source="stamen",scale=1)
ggmap(map)+geom_jitter(data = data, 
  aes(x=long,y=lat,color=tag,size=0.5,alpha=.5))+
  ggtitle("Hawks Location")+xlab('Longitude')+ylab('Latitude')
range(data$long)

#Q2
arrival <- subset(data, stage=="arrival")
arrival1 <- subset(arrival, tag == '105936')
arrival2 <- subset(arrival, tag == '105928')
loch1<-arrival1[,c(3,4)]
loch2<-arrival2[,c(3,4)]
L<-cbind(loch1$long,loch1$lat)
L2<-cbind(L[-nrow(L),],L[-1,])
colnames(L2)<-c("long1","lat1","long2","lat2")
L2<-data.frame(L2)

map2<-get_map(loch1,zoom=15,scale = 2)
ggmap(map2)+geom_point(data=arrival1, aes(x=long,y=lat,size=height,color=speed))+
  geom_segment(data=L2,aes(x=long1,y=lat1,xend=long2,yend=lat2),
                         arrow = arrow())+
  ggtitle("hawk105936")+xlab('Longitude')+ylab('Latitude')

L3<-cbind(loch2$long,loch2$lat)
L4<-cbind(L3[-nrow(L3),],L3[-1,])
colnames(L4)<-c("long1","lat1","long2","lat2")
L4<-data.frame(L4)

map3<-get_map(loch2,zoom=15)
ggmap(map3)+geom_point(data=arrival2, aes(x=long,y=lat,size=height,color=speed))+
  geom_segment(data=L4,aes(x=long1,y=lat1,xend=long2,yend=lat2),
               arrow = arrow())+
  ggtitle("hawk105928")+xlab('Longitude')+ylab('Latitude')


#Q3
data$time<-mdy_hm(data$time)
hawks<-split(data,tag)
nestlong<-aggregate(long~tag,data,mean)
nestlat<-aggregate(lat~tag,data,mean)
nests<-merge(nestlong,nestlat,by="tag")
levels(data$tag)
hawk1loc<-cbind(hawks[[1]]$long,hawks[[1]]$lat)
hawk1nest<-nests[1,c(2,3)]
dist1<-distGeo(hawk1loc,hawk1nest)
dist1<-data.frame(dist1)
dist1<-cbind(dist1,hawks[[1]])
ggplot(aes(x = mdy_hm(time), y = dist1/1609), data = dist1) + 
  geom_point()+geom_line()+
  geom_hline(yintercept = 7.5,color = "blue", size=1,lty="dashed")+
  ggtitle("hawk105923")+xlab('date')+ylab('Distance(mile)')+
  theme_bw()

hawk2loc<-cbind(hawks[[2]]$long,hawks[[2]]$lat)
hawk2nest<-nests[2,c(2,3)]
dist2<-distGeo(hawk2loc,hawk2nest)
dist2<-data.frame(dist2)
dist2<-cbind(dist2,hawks[[2]])
ggplot(aes(x = mdy_hm(time), y = dist2/1609), data = dist2) + 
  geom_point()+geom_line()+
  geom_hline(yintercept = 7.5,color = "blue", size=1,lty="dashed")+
  ggtitle("hawk105928")+xlab('date')+ylab('Distance(mile)')

hawk3loc<-cbind(hawks[[3]]$long,hawks[[3]]$lat)
hawk3nest<-nests[3,c(2,3)]
dist3<-distGeo(hawk3loc,hawk3nest)
dist3<-data.frame(dist3)
dist3<-cbind(dist3,hawks[[3]])
ggplot(aes(x = mdy_hm(time), y = dist3/1609), data = dist3) + 
  geom_point()+geom_line()+
  geom_hline(yintercept = 7.5,color = "blue", size=1,lty="dashed")+
  ggtitle("hawk105930")+xlab('date')+ylab('Distance(mile)')

hawk4loc<-cbind(hawks[[4]]$long,hawks[[4]]$lat)
hawk4nest<-nests[4,c(2,3)]
dist4<-distGeo(hawk4loc,hawk4nest)
dist4<-data.frame(dist4)
dist4<-cbind(dist4,hawks[[4]])
ggplot(aes(x = mdy_hm(time), y = dist4/1609), data = dist4) + 
  geom_point()+geom_line()+
  geom_hline(yintercept = 7.5,color = "blue", size=1,lty="dashed")+
  ggtitle("hawk105936")+xlab('date')+ylab('Distance(mile)')

hawk5loc<-cbind(hawks[[5]]$long,hawks[[5]]$lat)
hawk5nest<-nests[5,c(2,3)]
dist5<-distGeo(hawk5loc,hawk5nest)
dist5<-data.frame(dist5)
dist5<-cbind(dist5,hawks[[5]])
ggplot(aes(x = mdy_hm(time), y = dist5/1609), data = dist5) + 
  geom_point()+geom_line()+
  geom_hline(yintercept = 7.5,color = "blue", size=1,lty="dashed")+
  ggtitle("hawk117527")+xlab('date')+ylab('Distance(mile)')

#actually leave:4 2

#Q4
departure <- subset(data, stage=="preMigration")
table(departure$tag)

#departure sequence location of hawk 105928
departure1 <- subset(arrival, tag == '105928')
dloc1<-departure1[,c(3,4)]
L41<-cbind(dloc1$long,dloc1$lat)
L42<-cbind(L41[-nrow(L41),],L41[-1,])
colnames(L42)<-c("long1","lat1","long2","lat2")
L42<-data.frame(L42)
#departure sequence of hawk 105928
map4<-get_map(dloc1,zoom=15,scale = 2)
ggmap(map4)+geom_point(data=departure1, aes(x=long,y=lat,
                                            size=height,color=time,alpha=speed))+
  geom_segment(data=L42,aes(x=long1,y=lat1,xend=long2,yend=lat2),
               arrow = arrow())+
  ggtitle("hawk105928")+xlab('Longitude')+ylab('Latitude')

#departure sequence location of hawk 105936
departure2 <- subset(arrival, tag == '105936')
dloc2<-departure2[,c(3,4)]
L43<-cbind(dloc2$long,dloc2$lat)
L44<-cbind(L43[-nrow(L43),],L43[-1,])
colnames(L44)<-c("long1","lat1","long2","lat2")
L44<-data.frame(L44)
#departure sequence of hawk 105936
map5<-get_map(dloc2,zoom=15,scale=2)
ggmap(map5)+geom_point(data=departure2, aes(x=long,y=lat,
                                            size=height,color=time,alpha=speed))+
  geom_segment(data=L44,aes(x=long1,y=lat1,xend=long2,yend=lat2),
               arrow = arrow())+
  ggtitle("hawk1059236")+xlab('Longitude')+ylab('Latitude')






