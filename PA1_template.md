---title:"Reproducible Research: Peer Assessment 1"
authur:Xinran Huang
output:
  html_document
   keep_md:true
---

## Loading and preprocessing the data

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "activity.zip")
unzip("activity.zip")
activity <- read.csv("./activity.csv", header = T, stringsAsFactors = F)
activity[, 2] <- as.Date(activity[, 2])
```
## What is mean total number of steps taken per day?

```r
sum <- tapply(activity[, 1], activity[, 2], sum)  ##steps per day
hist(sum, breaks = 20, main = "Total number of steps taken each day", col = "red")
```

![plot of chunk unnamed-chunk-2](.pngunnamed-chunk-2-1.png) 
Histogram of the total number of steps taken each day.

```r
mean(activity[, 1], na.rm = T)
```

```
## [1] 37.3826
```

```r
median(activity[, 1], na.rm = T)
```

```
## [1] 0
```
Mean and Median of the total number of steps taken each day

## What is the average daily activity pattern?

```r
ts1 <- tapply(activity[!is.na(activity[, 1]), 1], activity[!is.na(activity[, 1]), 3], mean)
ts1_ts <- rownames(ts1)
ts1 <- substr(ts1, 1, 10)
plot(ts11, ts1_ts, type = "l", col = "blue", xlab = "Interval", ylab = "Number of Steps")  
```

```
## Error in plot(ts11, ts1_ts, type = "l", col = "blue", xlab = "Interval", : object 'ts11' not found
```
I don't think the order 'ts' can change anything meaningful here, so I don't use it.


```r
which.max(tapply(activity[!is.na(activity[, 1]), 1], activity[!is.na(activity[, 1]), 
    3], mean))
```

```
## 835 
## 104
```
The 104th Interval with index 835,on average across all the days in the dataset,contains the maximum number of steps.

## Imputing missing values

```r
missing <- sum(is.na(activity))
missing
```

```
## [1] 2304
```
The total number of missing values in the dataset is 2304. Calculate and report the total number of missing values in the dataset. Fill the missing values in the dataset with the mean for that 5-minute interval

```r
mean <- tapply(activity[, 1], activity[, 3], mean, na.rm = T)
mvp <- which(is.na(activity[, 1]))  ##missing value position
for (i in 1:length(mvp)) {
    activity[mvp[i], 1] <- mean[which(activity[mvp[i], 3] == rownames(mean))]
}
sum <- tapply(activity[, 1], activity[, 2], sum)
hist(sum, breaks = 20, main = "Total number of steps taken each day", col = "red")
```

![plot of chunk unnamed-chunk-7](.pngunnamed-chunk-7-1.png) 

```r
mean(activity[, 1])
```

```
## [1] 37.3826
```

```r
median(activity[, 1], na.rm = T)
```

```
## [1] 0
```
The histogram becomes larger. The median and mean is the same. This is because I use daily average in that 5-minutes interval to replace the missing date.

## Are there differences in activity patterns between weekdays and weekends? Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity$week <- weekdays(activity[, 2])
activity[which(activity[, 4] == "星期一" | activity[, 4] == "星期二" | activity[,4] == "星期三" | activity[, 4] == "星期四" | activity[, 4] == "星期五"),4] <- "Weekday"se Chinese because of this R version
activity[which(activity[, 4] == "星期六" | activity[, 4] == "星期日"), 4] <- "Weekend"
library("lattice")
ts2 <- tapply(activity$steps[activity$week == "Weekend"], activity$interval[activity$week == 
    "Weekend"], mean)
ts3 <- tapply(activity$steps[activity$week == "Weekday"], activity$interval[activity$week == 
    "Weekday"], mean)
ts2 <- cbind(ts2, week = "Weekday", rownames(ts2))
ts3 <- cbind(ts3, week = "Weekdend", rownames(ts3))
ts4 <- as.data.frame(rbind(ts2, ts3))
ts4[, 1] <- as.numeric(as.character(ts4[, 1]))
ts4[, 3] <- as.numeric(as.character(ts4[, 3]))
colnames(ts4) <- c("steps", "week", "interval")
with(ts4, xyplot(steps ~ interval | week, aspect = "xy", type = "l", ylab = "Number of steps"))
```

![plot of chunk unnamed-chunk-8](.pngunnamed-chunk-8-1.png) 
5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

 
 
