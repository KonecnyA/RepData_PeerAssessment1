# Reproducible Research: Peer Assessment 1
Andrew F Konecny  
December 5, 2016  


## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

## Data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

- **steps:** Number of steps taking in a 5-minute interval (missing values are coded as **NA**)
- **date:** The date on which the measurement was taken in YYYY-MM-DD format
- **interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Read Dataset

```r
## Set file.url with the data file link here.
file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## Download the file, get unzipped file name and unzip file.
download.file(file.url, destfile = "activity_monitoring_data.zip")
name.of.csv <- unzip("activity_monitoring_data.zip", list = TRUE)
unzip("activity_monitoring_data.zip")

## Load initial data frame. str confirmed 17,568 obs. and 3 variables. Convert string date to date class.
## 61 days x 288 intervals = 17,568 obs.
df.activity.monitoring.data <- read.csv(name.of.csv$Name, header = TRUE, sep = ",", stringsAsFactors = FALSE)
df.activity.monitoring.data$date <- as.Date(df.activity.monitoring.data$date, format="%Y-%m-%d")

## How many observations?
read.obs <- nrow(df.activity.monitoring.data)

## Nomissing (1st part of assignemnt) and missing version (for later part of assignment) data frames.
df.activity.monitoring.data.nomissing <- subset(df.activity.monitoring.data, (!is.na(steps)))
nomissing.steps <- nrow(df.activity.monitoring.data.nomissing)

df.activity.monitoring.data.missing <- subset(df.activity.monitoring.data, (is.na(steps)))
missing.steps <- nrow(df.activity.monitoring.data.missing)
```

17568 observations read into data frame with three variables: steps (int), date (Date) and interval (int). There are 15264 observations with no missing steps data and 2304 observations with missing steps data.

## Process Data

```r
library(doBy)
## Calculate Total (sum), Mean & Median on a non missing data frame.
df.statistics.steps.by.date.nomissing <- summaryBy(steps ~ date,
                                                   data =df.activity.monitoring.data.nomissing,
                                                   FUN = list(sum, mean, median),
                                                   na.rm = TRUE)

## Calculate Average (mean) steps by interval on a non missing data frame.
df.statistics.steps.by.interval.nomissing <- summaryBy(steps ~ interval,
                                                       data =df.activity.monitoring.data.nomissing,
                                                       FUN = list(mean),
                                                       na.rm = TRUE)
```

## What is mean total number of steps taken per day?

What is the difference between a histogram and a barplot? In a nutshell histograms show distributions of variables (quantitative data); whereas, barplots compare varaibles (catergorical / qualitative data).


```r
## Make a histogram of the total number of steps taken each day.
library(ggplot2)
ggplot(data = df.statistics.steps.by.date.nomissing, aes(x = date, y = steps.sum)) +
geom_bar(stat = "identity", fill = "steelblue") +
ggtitle("Total number of steps by Date (2012)") +
xlab("Date") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ylab("Total steps (no missing)")
```

![](PA1_template_files/figure-html/nomissing.histogram-1.png)<!-- -->

Of interest, one can see from the histogram that for some of the dates between October 1st, 2012 to November 30th, 2012 there are no observations.

## Report the mean and median total steps per day (missing excluded)

```r
knitr::kable(subset(df.statistics.steps.by.date.nomissing, select = -c(steps.sum)), digits = 2, padding = 0)
```



date        steps.mean  steps.median
----------  ----------  ------------
2012-10-02        0.44             0
2012-10-03       39.42             0
2012-10-04       42.07             0
2012-10-05       46.16             0
2012-10-06       53.54             0
2012-10-07       38.25             0
2012-10-09       44.48             0
2012-10-10       34.38             0
2012-10-11       35.78             0
2012-10-12       60.35             0
2012-10-13       43.15             0
2012-10-14       52.42             0
2012-10-15       35.20             0
2012-10-16       52.38             0
2012-10-17       46.71             0
2012-10-18       34.92             0
2012-10-19       41.07             0
2012-10-20       36.09             0
2012-10-21       30.63             0
2012-10-22       46.74             0
2012-10-23       30.97             0
2012-10-24       29.01             0
2012-10-25        8.65             0
2012-10-26       23.53             0
2012-10-27       35.14             0
2012-10-28       39.78             0
2012-10-29       17.42             0
2012-10-30       34.09             0
2012-10-31       53.52             0
2012-11-02       36.81             0
2012-11-03       36.70             0
2012-11-05       36.25             0
2012-11-06       28.94             0
2012-11-07       44.73             0
2012-11-08       11.18             0
2012-11-11       43.78             0
2012-11-12       37.38             0
2012-11-13       25.47             0
2012-11-15        0.14             0
2012-11-16       18.89             0
2012-11-17       49.79             0
2012-11-18       52.47             0
2012-11-19       30.70             0
2012-11-20       15.53             0
2012-11-21       44.40             0
2012-11-22       70.93             0
2012-11-23       73.59             0
2012-11-24       50.27             0
2012-11-25       41.09             0
2012-11-26       38.76             0
2012-11-27       47.38             0
2012-11-28       35.36             0
2012-11-29       24.47             0

Of interest, the median for all observations is 0.  This implies that 50 percent or more of the measurements are zero on any given day.

## What is the average daily activity pattern?

```r
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and
## the average number of steps taken, averaged across all days (y-axis).
ggplot(df.statistics.steps.by.interval.nomissing, aes(interval, steps.mean)) +
geom_line(colour = "steelblue", size = .8) +
ggtitle("Average number of steps taken by Interval (Oct - Nov 2012)") +    
xlab("5-minute interval") +
ylab("Average number of steps taken (across all days)")
```

![](PA1_template_files/figure-html/timeseries.plot-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps (missing excluded)

```r
## Which observation has the maximum number of steps?
max.avg.steps.obs <- which.max(df.statistics.steps.by.interval.nomissing$steps.mean)

## Set the maximum interval for including in text.
max.avg.interval <- df.statistics.steps.by.interval.nomissing[max.avg.steps.obs, "interval"]

## Set the maximum steps and round to an integer for including in text.
max.avg.steps <- round(df.statistics.steps.by.interval.nomissing[max.avg.steps.obs, "steps.mean"] + .5)
```
The 5-minute interval: 835, on average across all days, contains the maximum number of steps: 207 rounded up.


The total number of missing values (observations) in the data set is: 2304.

## Imputing missing values

```r
## I will replace the missing value with the mean total steps for that interval rounded up.
## Creating a new dataset that is equal to the original dataset but with missing data filled in.
for (i in seq(1, nrow(df.activity.monitoring.data), 288)) {
    if (is.na(df.activity.monitoring.data[i,"steps"])) {
        for (j in 1:288) {
            df.activity.monitoring.data[i+j-1,"steps"] = df.statistics.steps.by.interval.nomissing[j, "steps.mean"]
        }
    } else {
        for (j in 1:288) {
            df.activity.monitoring.data[i+j-1,"steps"] = df.activity.monitoring.data[i+j-1,"steps"]
        }
    }
}

## Calculate Total (sum), Mean & Median on an imputed data frame.
df.statistics.steps.by.date.imputed <- summaryBy(steps ~ date,
                                                 data = df.activity.monitoring.data,
                                                 FUN = list(sum, mean, median),
                                                 na.rm=TRUE)

## Calculate Average (mean) steps by interval on an imputed data frame.
df.statistics.steps.by.interval.imputed <- summaryBy(steps ~ interval + date,
                                                     data = df.activity.monitoring.data,
                                                     FUN = list(mean),
                                                     na.rm = TRUE)
```

## Histogram - Total steps each day after missing values are imputed

```r
## Make a histogram of the total number of steps taken each day.
ggplot(data = df.statistics.steps.by.date.imputed, aes(x = date, y = steps.sum)) +
geom_bar(stat = "identity", fill = "steelblue") +
ggtitle("Total number of steps by Date (2012)") +
xlab("Date") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ylab("Total steps (imputed)")
```

![](PA1_template_files/figure-html/impute.histogram-1.png)<!-- -->

Of interest, one can see from the histogram that that missing dates now have observations and the bars for the other dates look the same.

## Report the mean and median total steps per day after missing values are imputed

```r
## Report the mean and median total number of steps taken per day.
knitr::kable(subset(df.statistics.steps.by.date.imputed, select = -c(steps.sum)), digits = 2, padding = 0)
```



date        steps.mean  steps.median
----------  ----------  ------------
2012-10-01       37.38         34.11
2012-10-02        0.44          0.00
2012-10-03       39.42          0.00
2012-10-04       42.07          0.00
2012-10-05       46.16          0.00
2012-10-06       53.54          0.00
2012-10-07       38.25          0.00
2012-10-08       37.38         34.11
2012-10-09       44.48          0.00
2012-10-10       34.38          0.00
2012-10-11       35.78          0.00
2012-10-12       60.35          0.00
2012-10-13       43.15          0.00
2012-10-14       52.42          0.00
2012-10-15       35.20          0.00
2012-10-16       52.38          0.00
2012-10-17       46.71          0.00
2012-10-18       34.92          0.00
2012-10-19       41.07          0.00
2012-10-20       36.09          0.00
2012-10-21       30.63          0.00
2012-10-22       46.74          0.00
2012-10-23       30.97          0.00
2012-10-24       29.01          0.00
2012-10-25        8.65          0.00
2012-10-26       23.53          0.00
2012-10-27       35.14          0.00
2012-10-28       39.78          0.00
2012-10-29       17.42          0.00
2012-10-30       34.09          0.00
2012-10-31       53.52          0.00
2012-11-01       37.38         34.11
2012-11-02       36.81          0.00
2012-11-03       36.70          0.00
2012-11-04       37.38         34.11
2012-11-05       36.25          0.00
2012-11-06       28.94          0.00
2012-11-07       44.73          0.00
2012-11-08       11.18          0.00
2012-11-09       37.38         34.11
2012-11-10       37.38         34.11
2012-11-11       43.78          0.00
2012-11-12       37.38          0.00
2012-11-13       25.47          0.00
2012-11-14       37.38         34.11
2012-11-15        0.14          0.00
2012-11-16       18.89          0.00
2012-11-17       49.79          0.00
2012-11-18       52.47          0.00
2012-11-19       30.70          0.00
2012-11-20       15.53          0.00
2012-11-21       44.40          0.00
2012-11-22       70.93          0.00
2012-11-23       73.59          0.00
2012-11-24       50.27          0.00
2012-11-25       41.09          0.00
2012-11-26       38.76          0.00
2012-11-27       47.38          0.00
2012-11-28       35.36          0.00
2012-11-29       24.47          0.00
2012-11-30       37.38         34.11

```r
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
```

In building this report, I iteratively explored the no missing and imputed data frames.  The no missing has 53 observations which means some dates from October 1st, 2012 to November 30th, 2012 have missing data.  The imputed has 61 observations which means that all the dates are covered.  For the 53 dates that had data, the data is identical on the imputed data frame.  The only differences between the imputed data frame and the no missing data frame is that observations have been imputed for the 8 missing days.  This means that the Total steps overall would increase.  This further means that the data imputed to the 8 missing days is identical - based on my methodology.  For these days the average daily total would be the same which is a total of the mean intervals as calculated from the no missing data frame.  A final artifact is that there is now a median for these 8 days because there is no 0 observations.  I think these observations show some weaknesses of the very simple imputation method.


```r
## Might be better to plot the difference between the two.
##ggplot() + 
##geom_line(data=df.statistics.steps.by.date.imputed, aes(x=date, y=steps.sum), color='green') + 
##geom_line(data=df.statistics.steps.by.date.nomissing, aes(x=date, y=steps.sum), color='red')
```

## Are there differences in activity patterns between weekdays and weekends?

```r
##
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

for (i in 1:nrow(df.statistics.steps.by.interval.imputed)) {

        if (weekdays(df.statistics.steps.by.interval.imputed[i,"date"]) %in% weekdays1) {
            df.statistics.steps.by.interval.imputed[i, "MF.or.SS"] <- 'weekday'
        } else {
            df.statistics.steps.by.interval.imputed[i, "MF.or.SS"] <- 'weekend'
        }
}

library(lattice)
attach(df.statistics.steps.by.interval.imputed)
xyplot(steps.mean ~ interval | factor(MF.or.SS),
       type = 'l',layout = c(1,2),
       xlab = 'Interval',ylab = 'Number of Steps')
```

![](PA1_template_files/figure-html/panel.plot-1.png)<!-- -->
