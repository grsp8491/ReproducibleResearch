Reproducible Research: Course Project 1
====================================
October 2016

Always make code accessible

```r
echo = TRUE
```
###Loading and preprocessing the data  

```r
# Load libraries used in the analysis 
library(dplyr)
library(readr)
library(ggplot2)
```

```r
# download zipped file and unzip in Data directory
# use read_csv function from readr package
act.df <- read_csv("./Data/activity.csv", col_types = "iDi")
```
###What is mean total number of steps taken per day?
Ignore missing values.  

Make a histogram of the total number of steps taken each day


```r
act.df %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(TotalSteps = sum(steps)) %>%
        ggplot(., aes( x = TotalSteps)) +
        geom_histogram(binwidth = 1000, colour="black", fill = "steelblue", alpha=0.5, boundary=0) +
        
        ggtitle("Total number of steps taken each day\nin October and November 2012") +
        theme(plot.title=element_text(size=rel(1.3), face = 2, colour="grey30", margin = margin(t=10, b=5))) +
        
        xlab("Total steps per day") +
        theme(axis.title.x=element_text(size=rel(1.2), face=1, colour="grey30", margin = margin(t=20, b=10))) +
        scale_x_continuous(limits=c(0,25000), breaks=seq(from=0, to=25000, by=5000)) +  
        
        ylab("Number of days") +
        theme(axis.title.y=element_text(size=rel(1.2), face=1, colour="grey30", margin=margin(r=20, l=10))) +
        scale_y_continuous(limits=c(0,12), breaks=seq(from=0, to=12, by=2)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###The mean and median of the total number of steps taken per day

```r
act.df %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(TotalSteps = sum(steps)) %>%
        summarise(MeanStepsPerDay = mean(TotalSteps), MedianStepsPerDay = median(TotalSteps))
```

```
## # A tibble: 1 × 2
##   MeanStepsPerDay MedianStepsPerDay
##             <dbl>             <int>
## 1        10766.19             10765
```
###What is the average daily activity pattern?

```r
act.df %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(AvSteps = mean(steps)) %>%
        ggplot(., aes(x = interval, y = AvSteps)) +
        geom_line() +
        ggtitle("Average number of steps taken") +
        xlab("5-minute interval") +
        ylab("Average steps per 5-minute  interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
av.steps.df <- act.df %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(AvSteps = mean(steps))

maxav <- av.steps.df[which.max(av.steps.df$AvSteps), 1]
```
The 5-minute interval with the maximum number of steps is 835

###Imputing missing values


```r
MissingSteps <- sum(is.na(act.df$steps))
```
There are 2,304 missing values in the dataset.  
My strategy for filling in all of the missing values in the dataset is to use the mean for that 5-minute interval to fill the NAs in the step column.


```r
act2.df <- act.df %>%
        left_join(av.steps.df, by = "interval")

act2.df$adjsteps <- with(act2.df, ifelse(is.na(steps), AvSteps, steps))
```
###Make a histogram of the total number of steps taken each day.


```r
act2.df %>%
        group_by(date) %>%
        summarise(TotalSteps = sum(adjsteps)) %>%
        ggplot(., aes( x = TotalSteps)) +
        geom_histogram(binwidth = 1000, colour="black", fill = "steelblue", alpha=0.5, boundary=0) +
        
        ggtitle("Total number of steps taken each day\nin October and November 2012") +
        theme(plot.title=element_text(size=rel(1.3), face = 2, colour="grey30", margin = margin(t=10, b=5))) +
        
        xlab("Total steps per day") +
        theme(axis.title.x=element_text(size=rel(1.2), face=1, colour="grey30", margin = margin(t=20, b=10))) +
        scale_x_continuous(limits=c(0,25000), breaks=seq(from=0, to=25000, by=5000)) +  
        
        ylab("Number of days") +
        theme(axis.title.y=element_text(size=rel(1.2), face=1, colour="grey30", margin=margin(r=20, l=10))) +
        scale_y_continuous(limits=c(0,20), breaks=seq(from=0, to=20, by=5))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
###The mean and median of the total number of steps taken per day

```r
act2.df %>%
        group_by(date) %>%
        summarise(TotalSteps = sum(adjsteps)) %>%
        summarise(MeanStepsPerDay = mean(TotalSteps), MedianStepsPerDay = median(TotalSteps))
```

```
## # A tibble: 1 × 2
##   MeanStepsPerDay MedianStepsPerDay
##             <dbl>             <dbl>
## 1        10766.19          10766.19
```
The mean steps has remained unchanged.  The median steps has increased slightly and is now the same as the mean.


###Are there differences in activity patterns between weekdays and weekends?

```r
act2.df$day <- weekdays(act2.df$date)
weekend <- c("Saturday", "Sunday")
act2.df$DayType <- with(act2.df, ifelse(day %in% weekend, "Weekend", "Weekday"))
act2.df$DayType <- as.factor(act2.df$DayType)

act2.df %>%
        group_by(DayType, interval) %>%
        summarise(AvSteps = mean(adjsteps)) %>%
        ggplot(., aes(x = interval, y = AvSteps)) +
        geom_line() +
        facet_grid(DayType ~ ., scales="free") +
        ggtitle("Average number of steps taken") +
        xlab("5-minute interval") +
        ylab("Average steps per 5-minute  interval")   
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->




