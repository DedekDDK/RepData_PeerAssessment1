# Reproducible Research: Peer Assessment 1
####David Konvalina

This is an assignment to complete the course Reproducible research, where we need to make some data analysis and answer defined questions in the assignment.
This is a script which is answering all the questions and is documented via Rmd, Markdown and with knitr converted to HTML.

## Loading and preprocessing the data
First, data need to be unzipped and loaded into the R:

```r
unzip("activity.zip")
data<-read.csv("activity.csv")
```

Then, *date* field need to be converted to a Date type:

```r
data$date<-as.Date(data$date)
```
Now, the dataset should be ready for analysis:

```r
dim(data)
```

```
## [1] 17568     3
```

```r
summary(data)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
In order to find *mean total number of steps taken per day*, we need to aggregate total number of steps per date, then run **mean** and **median** function (ignoring NAs):

```r
steps_per_day<-aggregate(data$steps,by=list(data$date),FUN=sum)
names(steps_per_day)<-c("date","steps")
mean(steps_per_day$steps,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(steps_per_day$steps,na.rm=TRUE)
```

```
## [1] 10765
```

Here is the histogram of total number of steps per day:

```r
hist(steps_per_day$steps,main="Histogram of total number of steps per day",xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 



## What is the average daily activity pattern?
Typical daily activity pattern is the average of number of steps at specified 5min interval across all the days in the dataset.
First step is to group data by *interval*, and make mean of it.

```r
steps_per_interval<-aggregate(data$steps,by=list(data$interval),FUN=mean,na.rm=TRUE)
names(steps_per_interval)<-c("interval","steps")
plot(steps_per_interval$interval,steps_per_interval$steps,type="l",xlab="Interval",ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

Another task was to find out which interval contains the maximum number of steps:

```r
max_interval<-which(steps_per_interval$steps == max(steps_per_interval$steps))
steps_per_interval[max_interval,]
```

```
##     interval steps
## 104      835 206.2
```

As we could see, the maximum number of steps (206.1698) is at interval 835.


## Imputing missing values
As missing values could influence our calculations, we would like to get rid of them.
First step is to find out, how many of them we have.

```r
logna<-is.na(data$steps)
length(data[logna,1])
```

```
## [1] 2304
```

Missing data will be replaced by the mean value for the particular interval accross all the days:

```r
datamod<-data
ind<-1:nrow(datamod)
for(i in ind) {
        if(is.na(datamod$steps[i])) {
                datamod$steps[i] <- steps_per_interval$steps[steps_per_interval$interval==datamod$interval[i]]
        }
}
```

Now, we would like to see the difference of the original and dataset without NAs.

```r
steps_per_daymod<-aggregate(datamod$steps,by=list(datamod$date),FUN=sum)
names(steps_per_daymod)<-c("date","steps")
mean(steps_per_daymod$steps,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(steps_per_daymod$steps,na.rm=TRUE)
```

```
## [1] 10766
```

Here is the histogram of total number of steps per day:

```r
hist(steps_per_daymod$steps,main="Histogram of total number of steps per day",xlab="Total number of steps per day")
```

![plot of chunk unnamed-chunk-11](./PA1_template_files/figure-html/unnamed-chunk-11.png) 

On above we see that Mean is the same, Median is nearly the same and histogram is affected only that few values are more frequent, but in the same ratio.


## Are there differences in activity patterns between weekdays and weekends?

We want to find out, if there are some activity pattern differences between weekdays and weekends.
We have extended the dataset with variables of wday with abbreviation of week day, logical variable weekday and weekend, indicating if the wday is weekend of or week day.

```r
datamod$wday<-weekdays(datamod$date,abbreviate=TRUE)
datamod$weekend<-datamod$wday=="so" | datamod$wday=="ne"
datamod$weekday<-!datamod$weekend

data_aggr_weekday<-aggregate(datamod$steps[datamod$weekday==TRUE],by=list(datamod$interval[datamod$weekday==TRUE]),FUN=mean)
data_aggr_weekend<-aggregate(datamod$steps[datamod$weekend==TRUE],by=list(datamod$interval[datamod$weekend==TRUE]),FUN=mean)

data_aggr<-data_aggr_weekday
data_aggr<-cbind(data_aggr,data_aggr_weekend$x)
names(data_aggr)<-c("interval","weekday","weekend")
```

Here is the comparison of the number of steps over the day interval in weekday or weekend.

```r
par(fin=c(8,5))
par(mfrow=c(2,1))

plot(data_aggr$interval,data_aggr$weekday,type="l",ylab="Number of steps",xlab="Interval on weekday")

plot(data_aggr$interval,data_aggr$weekend,type="l",ylab="Number of steps",xlab="Interval on weekend")
```

![plot of chunk unnamed-chunk-13](./PA1_template_files/figure-html/unnamed-chunk-13.png) 

As the results we see that on weekdays that activity starts earlier, already right after 5am, whereas on the weekend, it starts about 9am.

Also, there is much more activity betweem 10am and 18am, as well as after 8pm (interval 2000).

