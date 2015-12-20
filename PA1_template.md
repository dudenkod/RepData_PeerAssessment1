# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(data.table)
library(dplyr)
library(lattice)
library(tidyr)

getwd()
setwd("~/JHU-courses/Course-5-W2/RepData_PeerAssessment1")

unzip(zipfile="./activity.zip",exdir="./")

activity <- read.table(file="./activity.csv", header = TRUE, sep=",")
```

## What is mean total number of steps taken per day?

```r
activity_clean <- group_by(activity, date)

activity_clean <- filter(activity_clean, !is.na(steps))

total_per_day <- summarize(activity_clean, total_steps = sum(steps))

mean(total_per_day$total_steps)
```

```
## [1] 10766.19
```

```r
median(total_per_day$total_steps)
```

```
## [1] 10765
```

```r
how_many_days <- length(total_per_day$total_steps)

hist(total_per_day$total_steps, xlab="steps per day", ylab="Frequency", breaks = how_many_days, col="blue", main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/lineplot1-1.png) 
## What is the average daily activity pattern?

```r
activity_clean2 <- group_by(activity, interval)
activity_clean2 <- filter(activity_clean2, !is.na(steps))
avg_steps <-  summarize(activity_clean2, avg = mean(steps))

plot(avg_steps$interval, avg_steps$avg, type="l",
     xlab = "5-min Step Interval", ylab = "Average Number of Steps", main = "The Average Daily Activity Pattern",  col ="blue")
```

![](PA1_template_files/figure-html/lineplot2-1.png) 

```r
avg_steps[which(avg_steps$avg==max(avg_steps$avg)),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval      avg
##      (int)    (dbl)
## 1      835 206.1698
```

```r
## Imputing missing values

# Calculate and report the total number of missing values in the dataset
# (the total number of rows with NAs)

print(nrow(activity)-nrow(activity_clean2))
```

```
## [1] 2304
```
#Not giving any extra steps to the statistics.


```r
activity_clean3 <- activity
activity_clean3$steps[is.na(activity$steps)] = 0

activity_clean3 <- group_by(activity_clean3, date)

total_per_day3 <- summarize(activity_clean3, total_steps = sum(steps))

hist(total_per_day$total_steps, ylim=c(0,10.0), xlab="steps per day", ylab="Frequency", breaks = how_many_days, col="blue", main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/lineplot3-1.png) 

```r
hist(total_per_day3$total_steps, ylim=c(0,10.0), xlab="steps per day", ylab="Frequency", breaks = how_many_days, col="blue", main="Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/lineplot3-2.png) 

```r
mean(total_per_day3$total_steps)
```

```
## [1] 9354.23
```

```r
median(total_per_day3$total_steps)
```

```
## [1] 10395
```


## Are there differences in activity patterns between weekdays and weekends?

#This system call doesn't have sufficient priviledges
#Sys.setenv(LANG = "en_US.UTF-8")
#Sys.setlocale("LC_TIME", "English")


```r
activity_clean4 <- mutate(activity_clean3, WDAY = weekdays(as.Date(date)))
activity_clean4$DTYPE <-ifelse(!(activity_clean4$WDAY %in% c("Samstag","Sonntag")), "weekday", "weekend")

activity_clean4 <- group_by(activity_clean4, interval)
activity_wday <-  filter(activity_clean4, DTYPE == "weekday")
activity_wend <-  filter(activity_clean4, DTYPE == "weekend")
avg_wday <-  summarize(activity_wday, avg = mean(steps), DTYPE = "weekday")
avg_wend <-  summarize(activity_wend, avg = mean(steps), DTYPE = "weekend")

avg_week <- rbind(avg_wday, avg_wend)

xyplot(avg ~  interval | DTYPE, data = avg_week, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/lineplot4-1.png) 
