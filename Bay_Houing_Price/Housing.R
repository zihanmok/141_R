data=readRDS('housing.rds')
library(dplyr)
library(lubridate)
library(ggplot2)
library(MASS)
library(treemap)
library(tidyr)
library(maps)

#Q1
data<-tbl_df(data)
glimpse(data)
#converting data types
data$date<-ymd(data$date)
data$lsqft<-as.integer(data$lsqft)
data$price<-as.integer(data$price)
data$county<-as.factor(data$county)
data$street<-as.character(data$street)
data$zip<-as.factor(data$zip)
data$year<-as.integer(data$year)
summary(data$year)
#Iregularities: there are extremely large values in variable 'year'
#that doesn't make sense.
boxplot(data$year)
#Q2
#Because the U.S is founded in 1776 and we are in 2018, only the
#values between them are included
dat2<-subset(data, year >= 1776&year <= 2018)
range(dat2$year)
range(data$date)

#Q3
#mutate create 2 new variables
data<-mutate(data,yr=year(data$date),mon=month(data$date))
#avg_price over time
ggplot(data=data, aes(y=price,x=factor(mon),group=yr,color=factor(yr)))+
  stat_summary(fun.y = "mean", geom = "line")+
  xlab("Month")+ylab("Avg Housing Price")+labs(color="year")+
  ggtitle("Avg Price Over Time")+theme_bw()
#sales over time
ggplot(data=data, aes(mon))+
  scale_x_continuous(breaks=seq(1,12,1))+
  geom_histogram(aes(fill=factor(yr)),binwidth = 0.5)+
  #display count number
  stat_bin(binwidth = 0.5, geom="text", colour="black", size = 3.5, 
           aes(label=..count.., group = yr), vjust = 1)+
  ylab("Sales")+ggtitle("Sales Over Time")+
  scale_fill_discrete(name="Year")#change legend title

#Q4
#correct the typo in county column
data$county<-sub("county","County",data$county)
data$county<-sub("San Franciscoe","San Francisco",data$county)
data$county<-sub("Alpine","San Francisco",data$county)
data$county<-as.factor(data$county)
dat4<-filter(data, year(date)%in%c("2005","2004","2003"))
dat4$br<-cut(as.numeric(dat4$br), breaks = c(0,1,2,3,28),
             labels=c("1","2","3","4+"))
dat4$yr<-as.factor(year(dat4$date))

ggplot(data = na.omit(dat4), aes(y=price,x=yr,group=br,color = br))+
  stat_summary(fun.y = "mean", geom = "line")+
  facet_wrap(~county)+
  theme_bw()+xlab("year")+ylab("avg_price")+
  ggtitle("Avg_price w.r.t Year/Br/County")+theme_classic()

#Q5
#extract pair_wise city and county then unique the table,
#then extract the city has more than 1 county
city_county_pair<-unique(data[,c("city","county")])
cc<-table(city_county_pair)
which(rowSums(a)>1)
cc[which(rowSums(a)>1),]

#Q6
dat6<-subset(data,price>0&bsqft<10000)
model1<-lm(dat6$price~dat6$bsqft)
summary(model1)
par(mfrow=c(1,3))
plot(dat6$bsqft, dat6$price,xlab = "bsqft",ylab="price",
     main="scatter plot")
model1<-lm(dat6$price~dat6$bsqft)
abline(model1,col="red",lwd=2)
plot(model1,which=2)
plot(model1,which=1)
#boxcox trans
bc<-boxcox(dat6$price~dat6$bsqft)
arrange(tbl_df(bc),desc(y))
model2<-lm(dat6$price^(-0.061)~dat6$bsqft)
summary(model2)
par(mfrow=c(1,2))
plot(model2,which=1)
plot(model2,which=4)

#Q7 t test
#conduct t test with alpha = 0.01
model3<-lm(data$price~data$bsqft+data$lsqft)
model3_est<-coef(summary(model3))[,1]
model3_se<-coef(summary(model3))[,2]
t_star<-(model3_est[2]-model3_est[3])/(model3_se[2])
critical_val<-qt(0.90,19997)
t_star>=critical_val

#Q8 fit differet linear regression model based on different county
plot(1, type="n", xlab="bsqft", ylab="price", 
     xlim=range(data$bsqft,na.rm = TRUE),
     ylim=range(data$price,na.rm = TRUE),
     main="regression models")
split_county<-split(data,data$county)
models<-lapply(split_county,function(x)lm(price~bsqft,x))
# x represent the dataset in lapply.
regression_lines<-sapply(1:9, function(i) abline(coef(models[[i]]), col=i))
legend("bottomright",names(table(data$county)),
       col=1:9,lty=rep(1,9),cex = 0.7)

#Q9
sales<-aggregate(price~city+county,data,length)
avg_price<-aggregate(price~city+county,data,mean)
colnames(sales)[3]<-"sales"
colnames(avg_price)[3]<-"avg_price"
#merge two dataset with key "city" and "county"
price_sales<-merge(sales,avg_price,by=c("city","county"))
dim(price_sales)
price_sales<-price_sales%>%
  group_by(county)%>%
  top_n(n=3L,wt=sales)
dim(price_sales)
treemap(price_sales,
        index = c("county","city"),
        vColor="avg_price",
        vSize ="sales" ,
        type="value",
        title='top3 sales with avg_price in differetn county',
        align.labels=list(c("left","top"),
                          c("center","center")),
        format.legend = list(scientific = FALSE, big.mark = " "))


#Q10
SFdata<-subset(data,data$city=="San Francisco")
SFdata$long2<-round(SFdata$long,2)
SFdata$lat2<-round(SFdata$lat,2)
#factorize the long,lat ranges,form cells.
long2_range<-range(SFdata$long2,na.rm=TRUE)
long_seq<-seq(long2_range[1],long2_range[2],.01)
SFdata$longF<-factor(SFdata$long2,levels=long_seq)

lat2_range<-range(SFdata$lat2,na.rm = T)
lat_seq<-seq(lat2_range[1],lat2_range[2],.01)
SFdata$latF<-factor(SFdata$lat2,levels=lat_seq)
SF_agg<-aggregate(price~latF+longF,SFdata,
                  function(x)c(mean(x),length(x)),drop=FALSE)

SFhouse_prices<-matrix(SF_agg$price[,1],
                     nlevels(SFdata$longF),
                     nlevels(SFdata$latF),byrow=TRUE)
SFhouse_prices[SFhouse_prices==0]<-NA
SFhouse_sales<-matrix(SF_agg$price[,2],
                     nlevels(SFdata$longF),
                     nlevels(SFdata$latF),byrow=TRUE)
SFhouse_sales[SFhouse_sales==0]<-NA
#
sf_border=map('county','california,san francisco',plot = F)
image(x=long_seq,y=lat_seq,z=SFhouse_prices,xlim=c(-122.56,-122.3),
      ylim = c(37.7,37.82),
      xlab = "Longitude", ylab = "Latitude", 
      main = "SF Heatmap of Average Housing Prices")
lines(sf_border$x,sf_border$y)


image(x=long_seq,y=lat_seq,z=SFhouse_sales,xlim=c(-122.56,-122.3),
      ylim = c(37.7,37.82),
      xlab = "Longitude", ylab = "Latitude", 
      main = "SF Heatmap of Housing Sales")
lines(sf_border$x,sf_border$y)