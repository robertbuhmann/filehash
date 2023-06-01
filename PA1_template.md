---
title: "Week 2 Reproducible Research Project"
author: "Rob Buhmann"
date: "31/05/2023"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

Week 2 Reproducible Research Project
====================================

This Markdown file contains the necessary processing and summary of the activity data for the week 2 project.

### 1. Load data


```r
data <- read.csv("activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Format variables

The next step is to format the variables in the dataframe so they can be graphed appropriately.


```r
data$date <- as.Date(data$date, "%Y-%m-%d")
data$day <- factor(weekdays(data$date))
data$steps <- as.numeric(data$steps)
```

### 2. Histogram of the total steps per day

Next we will plot a histogram of the total number of steps taken per day


```r
totals <- tapply(data$steps, INDEX = data$date, FUN = sum, na.rm = T)

hist(totals, main = "Total steps per day",
     xlab = "Total steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### 3. Mean and median steps per day

Now that the histogram is plotted, lets look at the mean an median steps per day


```r
mean(totals)
```

```
## [1] 9354.23
```

```r
median(totals)
```

```
## [1] 10395
```

### 4. Time series plot of average steps taken

Next lets plot the mean steps per five minute time interval.


```r
times <- aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm = T)
library(ggplot2)
ggplot(data = times, aes(x = Group.1, y = x))+
  geom_line()+
  theme_classic()+
  scale_x_continuous(limits = c(0, 2400), breaks = c(600, 1200, 1800, 2400))+
  ylab("No. of steps")+
  xlab("Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

### 5. interval with the max number of steps on average

and let's have a look at the interval with the mx number of steps


```r
max_interval <- times[which.max(times$x),]
max_interval
```

```
##     Group.1        x
## 104     835 206.1698
```

Here we can see the max number of steps occur during interval 835 (approx 2pm)

### 6. impute missing data

The original data file contains many missing (NA) data points. Next we will replace these missing data points with the mean for time interval of that day.


```r
days <- factor(data$day)
data_complete <- data.frame(matrix(nrow = 0, ncol = 4))

for (i in levels(days)){
  temp <- data[data$day == i,]
  temp$steps[is.na(temp$steps)] <- mean(temp$steps, na.rm = T)
  data_complete <- rbind(data_complete, temp)
}
```

### 7. Histogram of total daily steps with complete data

Now lets re-plot the histogram of total steps per day with our new complete dataset.


```r
totals_complete <- tapply(data_complete$steps, 
                          INDEX = data_complete$date, FUN = sum)

hist(totals_complete, main = "Average steps taken per day",
     xlab = "Total steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

If we look at the mean and median total steps per day calculated from our completed data set we can see these are slightly different compared with the mean and median from the dataset containing missing data.


```r
mean(totals_complete)
```

```
## [1] 10821.21
```

```r
median(totals_complete)
```

```
## [1] 11015
```

### 8. Weekdays vs. weekends

Next lets have a look at the mean number of steps during each five minute interval on weekdays vs. weekends. First we will have to create a new factor variable telling us whetehr a particualr day is a weekday or weekend.


```r
data_complete$weekday_weekend <- NA
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
index <- which(data_complete$day %in% weekdays)
data_complete[index,]$weekday_weekend <- "weekday"
data_complete$weekday_weekend <- replace(data_complete$weekday_weekend,
                                         is.na(data_complete$weekday_weekend),
                                         "weekend")
```

Next we will aggregate step data for each interval across weekdays and weekends


```r
attach(data_complete)
```

```
## The following objects are masked from data_complete (pos = 3):
## 
##     date, day, interval, steps, weekday_weekend
```

```r
weekday_weekend_totals <- aggregate(steps~weekday_weekend + interval, 
                                    FUN = sum)
weekday_weekend_totals$weekday_weekend <- factor(weekday_weekend_totals$weekday_weekend)
```

Finally we will create a panel plot of the number of steps per five minute interval on weekdays and weekends.


```r
ggplot(data = weekday_weekend_totals, aes(x = interval, y = steps))+
  geom_line()+
  theme_classic()+
  facet_wrap(~weekday_weekend)+
  scale_x_continuous(limits = c(0, 2400), breaks = c(600, 1200, 1800, 2400))+
  ylab("Steps")+
  xlab("Interval (24hr time)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

Here we can see that the average steps during almost any given interval on a weekday is greater than on a weekend.
