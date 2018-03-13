---
title: "Reproducible Research, Week 2"
output:

  html_document:

    keep_md: TRUE
---



## Course Project 1

Step 1 - wheel out the libraries


```r
library(tidyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

Read in the file and work it with DPLYR, grouping by date.


```r
if (!exists("activity")){
     temp <- tempfile()
     download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
     unzip(temp)
     activity <- read.csv("activity.csv")
     unlink(temp)
     file.remove("activity.csv")
}
```

```
## [1] TRUE
```

```r
activity_tb <-tbl_df(activity) %>% group_by(date)
#simply REMOVE NA values for now...
activity_tb_NAREM <- activity_tb[!is.na(activity_tb$steps),]
```

We want to get the mean and median number of steps per interval. This leads us to a quandary - does this consider only intervals where activity occurs, or ALL intervals?
Let us just take both cases. Below is a histogran showing frequency of daily step sums for 5 given ranges.


```r
summarize_steps <- summarize(activity_tb_NAREM, mean_steps = mean(steps, na.rm=TRUE), 
                           mean_no_zero = mean(steps[! steps %in% 0],na.rm=TRUE),
                           median_steps = median(steps,na.rm = TRUE), median_no_zero
                           = median(steps[! steps %in% 0]), sum_steps = sum(steps))

g <- ggplot(data=summarize_steps, aes(sum_steps))
ggeom <-geom_histogram(bins = 6,col="red", fill="green",alpha =0.2)
glabs <- labs(title = "Frequency of Steps on a Given Day", x="Total Steps", y = "Count")
gtheme <- theme(plot.title = element_text(hjust = 0.5))
g+ggeom+glabs+gtheme
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->




```r
g <- ggplot(data=summarize_steps, aes(date,y = value, color = variable, group = 1))
ggeom1 <- geom_line(aes(y = mean_steps, col = "mean_steps",linetype = "mean_steps"))
ggeom2 <- geom_line(aes(y = mean_no_zero, col = "mean_no_zero",linetype = "mean_no_zero"))
ggeom3 <- geom_line(aes(y = median_steps, col = "median_steps",linetype = "median_steps"))
ggeom4 <- geom_line(aes(y = median_no_zero, col = "median_no_zero",linetype = "median_no_zero"))
glabs <- labs(title = "Mean/Median Steps for Given Day over Selected Time Intervals",
               x="Date", y = "Average Interval Step-Count")
gtheme <- theme(plot.title = element_text(hjust = 0.5, size = 12),axis.text.x = element_blank())
glegend1 <-scale_linetype_discrete(name = "Variable",breaks = c("mean_no_zero","mean_steps","median_no_zero",
               "median_steps"),labels=c("Mean (Zeros Removed)", "Mean", "Median(Zeros Removed)","Median"))
glegend2 <-scale_color_discrete(name = "Variable",breaks = c("mean_no_zero","mean_steps","median_no_zero",
               "median_steps"),labels=c("Mean (Zeros Removed)", "Mean", "Median(Zeros Removed)","Median"))
g+ggeom1+ggeom2+ggeom3+ggeom4+glabs+gtheme+glegend1+glegend2
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Note the median is meaningless without removal of zeros...the mean is considered for both cases - all activity, and intervals ONLY with activity.

## Average Daily Activity Pattern

Having taken care of the average per day, a perhaps more meaningful assessment is the average number of steps at a particular interval over the span of days (no need to deal with zeros in that case. Grouping by intervals instead of date solves this readily.



```r
activity_interval <-tbl_df(activity) %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE))
max_value <- activity_interval$mean_steps[which.max(activity_interval$mean_steps)]
g <- ggplot(data=activity_interval, aes(interval, mean_steps))
glabs <- labs(title = "Average Number of Steps for Given Time Interval", x="Interval", y = "Average Step-Count (over Observation Days)")
gnote <- annotate("text", x = 1000, y = 200, label = paste(c("max = ", as.character(max_value)),collapse = ""))
g+geom_line()+glabs+gnote
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Fixing missing Values

Now we consider missing values - our correction is to replace them with the interval-averaged values from all days observed.
The method here is to spread out the data frame over the intervals, take the column means (essentially duplicating the result of last section), then pick out the NA values and replace with the appropriate mean. After this we gather the columns together again.


```r
xx <- spread(activity, interval, steps)
xxmean <- xx %>% select(-date) %>% colMeans(na.rm = TRUE)

for (i in 1:nrow(xx)){
     xx[i,which(is.na(xx[i,]))] <-xxmean[which(is.na(xx[i,]))-1]
}
activity_fixNA <- arrange(gather(xx, interval, steps, -date),date) %>% tbl_df() %>% group_by(date)

summarize_steps_fix <- summarize(activity_fixNA, mean_steps = mean(steps, na.rm=TRUE), 
                           mean_no_zero = mean(steps[! steps %in% 0],na.rm=TRUE),
                           median_steps = median(steps,na.rm = TRUE), median_no_zero
                           = median(steps[! steps %in% 0]), sum_steps = sum(steps))
```

Best thing to do is overlay the plots. Will use base plotting for this one.


```r
hist_steps_fix <- hist(summarize_steps_fix$sum_steps, breaks = 6,col=rgb(1,0,0,0.5,alpha = 1),xlab = "Step Count", ylab = "Frequency", 
                       main = "Step-Count Frequency with and without NA-Replacement")
hist_steps <- hist(summarize_steps$sum_steps, breaks = 6,col=rgb(1,1,1,1,alpha = 0.8),add = TRUE)
box()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

We notice the averaging adds, as expected, additional counts to the center bin. 

Looking at the interval averages now:


```r
g <- ggplot(data=summarize_steps_fix, aes(date,y = value, color = variable, group = 1))
ggeom1 <- geom_line(aes(y = mean_steps, col = "mean_steps",linetype = "mean_steps"))
ggeom2 <- geom_line(aes(y = mean_no_zero, col = "mean_no_zero",linetype = "mean_no_zero"))
ggeom3 <- geom_line(aes(y = median_steps, col = "median_steps",linetype = "median_steps"))
ggeom4 <- geom_line(aes(y = median_no_zero, col = "median_no_zero",linetype = "median_no_zero"))
glabs <- labs(title = "Time-Interval Mean/Median Steps per Interval over Observation Days",
               x="Date", y = "Average Interval Step-Count")
gtheme <- theme(plot.title = element_text(hjust = 0.5, size = 12),axis.text.x = element_blank())
glegend1 <-scale_linetype_discrete(name = "Variable",breaks = c("mean_no_zero","mean_steps","median_no_zero",
               "median_steps"),labels=c("Mean (Zeros Removed)", "Mean", "Median(Zeros Removed)","Median"))
glegend2 <-scale_color_discrete(name = "Variable",breaks = c("mean_no_zero","mean_steps","median_no_zero",
               "median_steps"),labels=c("Mean (Zeros Removed)", "Mean", "Median(Zeros Removed)","Median"))
g+ggeom1+ggeom2+ggeom3+ggeom4+glabs+gtheme+glegend1+glegend2
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Not very illuminating unless we juxtapose or look at the difference. Doing "anti_join" shows us what contribution the imputing has - essentially 8 days of no activity which in the precision of our results contributes negligibly.


```r
x <- filter(summarize_steps_fix,summarize_steps_fix$date %in% summarize_steps$date)
anti_join(summarize_steps_fix,summarize_steps)
```

```
## Joining, by = c("date", "mean_steps", "mean_no_zero", "median_steps", "median_no_zero", "sum_steps")
```

```
## # A tibble: 8 x 6
##   date       mean_steps mean_no_zero median_steps median_no_zero sum_steps
##   <fct>           <dbl>        <dbl>        <dbl>          <dbl>     <dbl>
## 1 2012-10-01       37.4         40.0         34.1           37.5     10766
## 2 2012-10-08       37.4         40.0         34.1           37.5     10766
## 3 2012-11-01       37.4         40.0         34.1           37.5     10766
## 4 2012-11-04       37.4         40.0         34.1           37.5     10766
## 5 2012-11-09       37.4         40.0         34.1           37.5     10766
## 6 2012-11-10       37.4         40.0         34.1           37.5     10766
## 7 2012-11-14       37.4         40.0         34.1           37.5     10766
## 8 2012-11-30       37.4         40.0         34.1           37.5     10766
```

## Differences in Activity Patterns between Weekday and Weekend.


```r
weekday <- factor(c("Monday","Tuesday","Wednesday","Thursday","Friday"))
weekend <- factor(c("Saturday","Sunday"))

day_in_week <- weekdays(as.Date(activity_fixNA$date))

daytype <- character()
daytype[day_in_week %in% weekday] <- "weekday"
daytype[!day_in_week %in% weekday] <- "weekend"
```

This produces our factors now we produce the plot


```r
activity_wd <- tbl_df(cbind.data.frame(activity_fixNA,daytype)) %>% mutate(date,weekdays(as.Date(date))) %>% group_by(interval,daytype)
summarize_steps_wd <- summarize(activity_wd, mean_steps = mean(steps), 
     mean_no_zero = mean(steps[! steps %in% 0]),median_steps = median(steps[!steps %in% 0]), sum_steps = sum(steps))

p <- ggplot(data=summarize_steps_wd,aes(interval,mean_steps,group=1))+geom_line(inherit.aes = TRUE)
plabs <- labs(title = "Average Step Count for given Interval", x="Interval", y = "Step Count")
p+facet_grid(daytype ~.)+plabs
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


Looking at weekend activity, we see see diminished activity at the end of the time interval and higher amount in the middle of the interval.Assuming the flatline is the time during which the person is sleeping, this suggests earlier rise time and more activity at waking hours during weekday and higher activity later in the evening on weekends.
