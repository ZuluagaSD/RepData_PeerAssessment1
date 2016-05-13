# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
## Unzip the file and read in one line

activitydata <- read.csv(unzip("activity.zip"), sep = ",")

## Format the date column as.Date

activitydata$date <- as.Date(activitydata$date)

head(activitydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?


```r
## Load dplyr package to summarise the number of steps and ggplot2 for the plot

library(dplyr)
library(ggplot2)

## 1. Calculate the total number of steps taken per day

## Create a new data set without NAs

actdata <- activitydata[complete.cases(activitydata), ]

## Sum all the steps taked each day by summarising the data of each date ignoring dates that have NA values

sum.activitydata <- actdata %>%
    group_by(date) %>%
    summarise(stepsday = sum(steps)) %>%
    na.omit

head(sum.activitydata)
```

```
## Source: local data frame [6 x 2]
## 
##         date stepsday
##       (date)    (int)
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
## Plot the number of steps taken each day

qplot(x = sum.activitydata$stepsday, bins = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
## Calculate the mean and median of the summarised data

meansteps <- mean(sum.activitydata$stepsday)

mediansteps <- median(sum.activitydata$stepsday)

paste("Mean", as.integer(meansteps))
```

```
## [1] "Mean 10766"
```

```r
paste("Median", as.integer(mediansteps))
```

```
## [1] "Median 10765"
```


## What is the average daily activity pattern?


```r
## Sum all the steps taken in each 5-minute interval

mean.interval <- actdata %>%
    group_by(interval) %>%
    summarise(stepsinterval = mean(steps))
```

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
qplot(mean.interval$interval, mean.interval$stepsinterval, geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
