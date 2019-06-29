---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



Read in the dataset and then set the date column to a date class.
Load all required librarys.


```r
#read in the dataset

mydata <- read.csv(file="activity.csv")
mydata$date <- as.Date(mydata$date, format = "%Y-%m-%d")
library(dplyr)
library(knitr)
```

Total steps per day, mean per day and median per day
Using dplyr to group by day and then calculate the mean and median.

```r
group_by_day <- mydata %>%
  na.omit %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps),
            daily_mean = mean(steps),
            daily_median = median(steps))

print(c("Mean steps per day", mean(group_by_day$total_steps)))
```

```
## [1] "Mean steps per day" "10766.1886792453"
```

```r
print(c("Median steps per day", median(group_by_day$total_steps)))
```

```
## [1] "Median steps per day" "10765"
```

Create a histogram to show the amount of steps per day


```r
hist(group_by_day$total_steps, xlab = "Total Steps", main= "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Plot a comparison of average daily steps by interval.


```r
group_by_int <- mydata %>%
  na.omit %>%
  group_by(interval) %>%
  summarise(av_steps = mean(steps))

with(group_by_int, plot(x = interval, y = av_steps, type = "l", xlab = "5 minute interval", 
                        ylab = "Mean Steps", main = "Average steps by 5 minute interval" ))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5 minute interval has the highest average steps per day?


```r
print("Highest average steps per interval")
```

```
## [1] "Highest average steps per interval"
```

```r
head(group_by_int[rev(order(group_by_int$av_steps)),],1)
```

```
## # A tibble: 1 x 2
##   interval av_steps
##      <int>    <dbl>
## 1      835     206.
```

Total NA values in Dataset?


```r
sum(is.na(mydata))
```

```
## [1] 2304
```

Replace NA values with the average steps for that interval

```r
# for loop so that if NA value replace with average for given interval

complete = mydata
for (i in 1:nrow(complete)) {
  if (is.na(complete[i, "steps"])) {
    interval = as.numeric(complete[i, "interval"])
    complete[i, "steps"] = group_by_int[which(group_by_int== interval),2]
  }
}

#check all NA has been replaced
sum(is.na(complete))
```

```
## [1] 0
```

```r
#group new dataframe daily

group_by_day2 <- complete %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps),
            daily_mean = mean(steps),
            daily_median = median(steps))

# plot histogram of new data

hist(group_by_day2$total_steps, col="blue", xlab = "Total Steps", 
     main = "Total steps by date with no missing values")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
print("New Mean")
```

```
## [1] "New Mean"
```

```r
mean(group_by_day2$total_steps)
```

```
## [1] 10766.19
```

```r
print("New Median")
```

```
## [1] "New Median"
```

```r
median(group_by_day2$total_steps)
```

```
## [1] 10766.19
```

Are there differences in activity patterns between weekdays and weekends?

```r
# add a weekday column to dataset

complete$weekday <- weekdays(complete$date)
# add whether weekday or weekend
complete$wtype <- factor(ifelse(complete$weekday == "Saturday" | complete$weekday == "Sunday" , 
                         "Weekend", "Weekday"), levels = c("Weekend", "Weekday"))
# group by weekend/weekday
group_by_wtype <- complete %>%
  group_by(interval, wtype) %>%
  summarise(av_steps = mean(steps))

# set up plot area
par(mfrow=c(2,1),mar=c(4,3,1,1))

#plot weekend data
with(subset(group_by_wtype, wtype == "Weekend"), 
     plot(x = interval, y = av_steps, type = "l",
          xlab = "5 minute interval", ylab = "Mean Steps", 
          main = "Average Weekend steps by 5 minute interval", cex.main = 0.8))

#plot weekday data
with(subset(group_by_wtype, wtype == "Weekday"), 
     plot(x = interval, y = av_steps, type = "l",
          xlab = "5 minute interval", ylab = "Mean Steps", 
          main = "Average Weekday steps by 5 minute interval", cex.main = 0.8))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


