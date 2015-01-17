---
title: 'Reproducible Research: Peer Assessment 1'
author: "Dong dong"
date: "Saturday, January 10, 2015"
output: html_document
---

##Loading and preprocessing the data

```r
#load the required data and library
library(ggplot2)
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
options(scipen=5,digits=2)
data<-read.csv("./activity.csv")


#Aggregate the steps by date
sum_day<-aggregate(data$steps,by=list(date=data$date), sum,na.rm=T)
colnames(sum_day)<-c("date", "steps")
sum_day$date<-as.Date(sum_day$date)

#Aggregate the steps by time interval
mean_interval<-aggregate(data$steps, by=list(interval=data$interval),mean,na.rm=T)
colnames(mean_interval)<-c("interval", "steps")
```

##What is mean total number of steps taken per day?

```r
mean_steps=mean(sum_day$steps)
median_steps=median(sum_day$steps)
sum_day_plot<-ggplot(aes(x=date,y=steps),data=sum_day)
sum_day_plot+geom_histogram(stat="identity")+
geom_hline(y=mean_steps,colour="red",size=2,alpha=0.5)+
geom_hline(y=median_steps,colour="blue",size=2, alpha=0.5)+
annotate("text",x=as.Date("2012-10-01"),y=8500,label="Mean",colour="Red")+
annotate("text",x=as.Date("2012-10-01"),y=11500,label="Meadian",colour="Blue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

- The mean total number of steps taken per day is 9354.23.  
- The median total number of steps taken per day is 10395.  

##What is the average daily activity pattern?

```r
max_steps=max(mean_interval$steps)
max_mean_interval<-mean_interval[which(mean_interval$steps==max_steps),]
mean_interval_plot<-ggplot(aes(x=interval,y=steps),data=mean_interval)
mean_interval_plot+geom_line(type="l",colour="Green",size=2)+
geom_vline(x=max_mean_interval$interval,colour="Blue",size=1,alpha=0.5,linetype=3)+
annotate("text",x=1000, y=200,label="Max",colour="Red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

- On average across all days in the dataset,835 contains the maximum number of steps.  

##Imputing missing values

```r
sum_NA<-sum(is.na(data$steps))
data2<-data
for (i in 1:length(data2[,1])){
    if (is.na(data2[i,1]))
        {data2[i,1]<-mean_interval[which(mean_interval$interval==data2[i,3]),2]}
    else if (is.na(data2[i,1]==F))
        {data2[i,1]<-data2[i,1]}
    }
sum_day2<-aggregate(data2$steps,by=list(date=data2$date), sum,na.rm=T)
colnames(sum_day2)<-c("date", "steps")
sum_day2$date<-as.Date(sum_day2$date)
mean_steps2=mean(sum_day2$steps)
median_steps2=median(sum_day2$steps)
sum_day2_plot<-ggplot(aes(x=date,y=steps),data=sum_day2)
sum_day2_plot+geom_histogram(stat="identity")+
geom_hline(y=mean_steps2,colour="red",size=2,alpha=0.5)+
geom_hline(y=median_steps2,colour="blue",size=2, alpha=0.5)+
annotate("text",x=as.Date("2012-10-01"),y=8500,label="Mean",colour="Red")+
annotate("text",x=as.Date("2012-10-01"),y=11500,label="Meadian",colour="Blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

- The total number of missing values in the dataset is 2304.  
- The NA values were filled in using the corresponding mean of 5-min interval steps. A new dataset was formed. 
- The mean total number of steps taken per day of the new dataset is 10766.19.    
- The median total number of steps taken per day of the new dataset is 10766.19.   
- The mean and median total number of steps taken per day of the new dataset is different from the original one. Inputting the missing data, the number is a little larger than before.

##Are there differences in activity patterns between weekdays and weekends?

```r
data$weekdays<-weekdays(as.Date(data$date))
for (i in 1:length(data[,2])){
    if (grepl("Saturday|Sunday",data$weekdays[i])){
        data$weekdays[i]<-"Weekend"
        }else{
        data$weekdays[i]<-"Weekday"
        }
        }
attach(data)
mean_interval_weekday<-aggregate(steps,by=list(interval,weekdays),sum,na.rm=T)
detach(data)
colnames(mean_interval_weekday)<-c("interval","weekday","steps")
mean_interval_weekday_plot<-ggplot(aes(x=interval,y=steps),data=mean_interval_weekday)
mean_interval_weekday_plot+geom_line(type="l",colour="Orange",size=1)+facet_grid(weekday~.)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

- From the photograph above, there is differences in activity patterns between weekdays and weekends.  
