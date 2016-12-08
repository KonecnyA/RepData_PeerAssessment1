# Analyzing activity monitoring (steps) from Oct. 1 to Nov. 30 2012
Andrew F Konecny  
December 8, 2016  



## 1. Synopsis ##

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/ "Fitbit"), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband "Nike Fuleband"), or [Jawbone Up](https://jawbone.com/up "Jawbone Up") . These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

## 2. Data Processing ##

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "Activity monitoring data") [52K]

The variables included in this dataset are:

- **steps:** Number of steps taking in a 5-minute interval (missing values are coded as **NA**)
- **date:** The date on which the measurement was taken in YYYY-MM-DD format
- **interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### 2.1 Load Data ###

```r
## Set file.url with the data file link here.
file.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## Download the file, get unzipped file name and unzip file.
download.file(file.url, destfile = "activity_monitoring_data.zip")
name.of.csv <- unzip("activity_monitoring_data.zip", list = TRUE)
unzip("activity_monitoring_data.zip")

## Load initial data frame.
## 61 days x 288 intervals = 17,568 obs.
df.activity.monitoring.data <- read.csv(name.of.csv$Name, header = TRUE, sep = ",", stringsAsFactors = FALSE)

## This will convert a 5 minute interval to a 24 hour time.
setTime <- function(y) {
    s <- "00"
    h <- sprintf("%02d", y%/%100)
    m <- sprintf("%02d", y%%100)
    return(paste(h,":",m,":",s, sep=""))
}

## Create a POSIXct object that holds date and time.
df.activity.monitoring.data$date <- as.POSIXct(paste(df.activity.monitoring.data$date,
                                                  " ",
                                                  setTime(df.activity.monitoring.data$interval),
                                                  sep=""), format="%Y-%m-%d %H:%M:%S")

## For plotting store ymd and time as seperate variables.
df.activity.monitoring.data$ymd <- date(df.activity.monitoring.data$date)
df.activity.monitoring.data$time <- format(df.activity.monitoring.data$date, format = "%H:%M")
df.activity.monitoring.data$time <- as.POSIXct(df.activity.monitoring.data$date, format = "%H:%M")

## How many observations and variables?
read.obs  <- nrow(df.activity.monitoring.data)
read.vars <- ncol(df.activity.monitoring.data)
var.names <- names(df.activity.monitoring.data)

## Nomissing (1st part of assignemnt) and missing version (for later part of assignment) data frames.
df.activity.monitoring.data.nomissing <- subset(df.activity.monitoring.data, (!is.na(steps)))
nomissing.steps <- nrow(df.activity.monitoring.data.nomissing)

df.activity.monitoring.data.missing <- subset(df.activity.monitoring.data, (is.na(steps)))
missing.steps <- nrow(df.activity.monitoring.data.missing)
```

17568 observations read into data frame with 5 variables: steps, date, interval, ymd, time (I added the last two for plotting purposes). There are 15264 observations with no missing steps data and 2304 observations with missing steps data.

## 2.2 Process Data ##

```r
## Calculate Total (sum), Mean & Median on a non missing data frame.
df.statistics.steps.by.date.nomissing <- summaryBy(steps ~ ymd,
                                                   data =df.activity.monitoring.data.nomissing,
                                                   FUN = list(sum, mean, median),
                                                   na.rm = TRUE)

## Calculate Average (mean) steps by interval on a non missing data frame.
df.statistics.steps.by.interval.nomissing <- summaryBy(steps ~ interval,
                                                       data =df.activity.monitoring.data.nomissing,
                                                       FUN = list(mean),
                                                       na.rm = TRUE)
```

## 3. Results ##
### 3.1 What is mean total number of steps taken per day? ###

What is the difference between a histogram and a barplot? In a nutshell histograms show distributions of variables (quantitative data); whereas, barplots compare varaibles (catergorical / qualitative data).


```r
## Make a histogram of the total number of steps taken each day.
ggplot(data = df.statistics.steps.by.date.nomissing, aes(x = ymd, y = steps.sum)) +
geom_bar(stat = "identity", fill = "steelblue") +
ggtitle("Total number of steps by Date (2012)") +
xlab("Date") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ylab("Total steps (no missing)")
```

![](PA1_template_files/figure-html/nomissing.histogram-1.png)<!-- -->

Of interest, one can see from the histogram that for some of the dates between October 1st, 2012 to November 30th, 2012 there are no observations.

### 3.2 Report the mean and median total steps per day. ###

```r
##knitr::kable(subset(df.statistics.steps.by.date.nomissing, select= -c(steps.sum)), digits = 2, padding = 0)

subset(df.statistics.steps.by.date.nomissing, select = -steps.sum)
```

```
##           ymd steps.mean steps.median
## 1  2012-10-02  0.4375000            0
## 2  2012-10-03 39.4166667            0
## 3  2012-10-04 42.0694444            0
## 4  2012-10-05 46.1597222            0
## 5  2012-10-06 53.5416667            0
## 6  2012-10-07 38.2465278            0
## 7  2012-10-09 44.4826389            0
## 8  2012-10-10 34.3750000            0
## 9  2012-10-11 35.7777778            0
## 10 2012-10-12 60.3541667            0
## 11 2012-10-13 43.1458333            0
## 12 2012-10-14 52.4236111            0
## 13 2012-10-15 35.2048611            0
## 14 2012-10-16 52.3750000            0
## 15 2012-10-17 46.7083333            0
## 16 2012-10-18 34.9166667            0
## 17 2012-10-19 41.0729167            0
## 18 2012-10-20 36.0937500            0
## 19 2012-10-21 30.6284722            0
## 20 2012-10-22 46.7361111            0
## 21 2012-10-23 30.9652778            0
## 22 2012-10-24 29.0104167            0
## 23 2012-10-25  8.6527778            0
## 24 2012-10-26 23.5347222            0
## 25 2012-10-27 35.1354167            0
## 26 2012-10-28 39.7847222            0
## 27 2012-10-29 17.4236111            0
## 28 2012-10-30 34.0937500            0
## 29 2012-10-31 53.5208333            0
## 30 2012-11-02 36.8055556            0
## 31 2012-11-03 36.7048611            0
## 32 2012-11-05 36.2465278            0
## 33 2012-11-06 28.9375000            0
## 34 2012-11-07 44.7326389            0
## 35 2012-11-08 11.1770833            0
## 36 2012-11-11 43.7777778            0
## 37 2012-11-12 37.3784722            0
## 38 2012-11-13 25.4722222            0
## 39 2012-11-15  0.1423611            0
## 40 2012-11-16 18.8923611            0
## 41 2012-11-17 49.7881944            0
## 42 2012-11-18 52.4652778            0
## 43 2012-11-19 30.6979167            0
## 44 2012-11-20 15.5277778            0
## 45 2012-11-21 44.3993056            0
## 46 2012-11-22 70.9270833            0
## 47 2012-11-23 73.5902778            0
## 48 2012-11-24 50.2708333            0
## 49 2012-11-25 41.0902778            0
## 50 2012-11-26 38.7569444            0
## 51 2012-11-27 47.3819444            0
## 52 2012-11-28 35.3576389            0
## 53 2012-11-29 24.4687500            0
```

Of interest, the median for all observations is 0.  This implies that 50 percent or more of the measurements are zero on any given day.

### 3.3 What is the average daily activity pattern? ###

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

### 3.4 The 5-minute interval that, on average, contains the maximum number of steps. ###

```r
## Which observation has the maximum number of steps?
max.avg.steps.obs <- which.max(df.statistics.steps.by.interval.nomissing$steps.mean)

## Set the maximum interval for including in text.
max.avg.interval <- df.statistics.steps.by.interval.nomissing[max.avg.steps.obs, "interval"]

## Set the maximum steps.
max.avg.steps <- df.statistics.steps.by.interval.nomissing[max.avg.steps.obs, "steps.mean"]
```
The 5-minute interval: 835, on average across all days, contains the maximum number of steps: 206.1698113.


The total number of missing values (observations) in the data set is: 2304.

### 3.5 Imputing missing values ###

```r
## I will replace the missing value with the mean total steps for that interval.
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
df.statistics.steps.by.date.imputed <- summaryBy(steps ~ ymd,
                                                 data = df.activity.monitoring.data,
                                                 FUN = list(sum, mean, median),
                                                 na.rm=TRUE)

## Calculate Average (mean) steps by interval on an imputed data frame.
df.statistics.steps.by.interval.imputed <- summaryBy(steps ~ interval + ymd,
                                                     data = df.activity.monitoring.data,
                                                     FUN = list(mean),
                                                     na.rm = TRUE)
```

### 3.6 Histogram - Total steps each day after missing values are imputed ###

```r
## Make a histogram of the total number of steps taken each day.
ggplot(data = df.statistics.steps.by.date.imputed, aes(x = ymd, y = steps.sum)) +
geom_bar(stat = "identity", fill = "steelblue") +
ggtitle("Total number of steps by Date (2012)") +
xlab("Date") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
ylab("Total steps (imputed)")
```

![](PA1_template_files/figure-html/impute.histogram-1.png)<!-- -->

Of interest, one can see from the histogram that the missing dates now have observations and the bars for the other dates look identical to those previously plotted.

### 3.7 Report the mean and median total steps per day after missing values are imputed ###

```r
## Report the mean and median total number of steps taken per day.
##knitr::kable(subset(df.statistics.steps.by.date.imputed, select = -c(steps.sum)), digits = 2, padding = 0)

subset(df.statistics.steps.by.date.imputed, select=-steps.sum)
```

```
##           ymd steps.mean steps.median
## 1  2012-10-01 37.3825996     34.11321
## 2  2012-10-02  0.4375000      0.00000
## 3  2012-10-03 39.4166667      0.00000
## 4  2012-10-04 42.0694444      0.00000
## 5  2012-10-05 46.1597222      0.00000
## 6  2012-10-06 53.5416667      0.00000
## 7  2012-10-07 38.2465278      0.00000
## 8  2012-10-08 37.3825996     34.11321
## 9  2012-10-09 44.4826389      0.00000
## 10 2012-10-10 34.3750000      0.00000
## 11 2012-10-11 35.7777778      0.00000
## 12 2012-10-12 60.3541667      0.00000
## 13 2012-10-13 43.1458333      0.00000
## 14 2012-10-14 52.4236111      0.00000
## 15 2012-10-15 35.2048611      0.00000
## 16 2012-10-16 52.3750000      0.00000
## 17 2012-10-17 46.7083333      0.00000
## 18 2012-10-18 34.9166667      0.00000
## 19 2012-10-19 41.0729167      0.00000
## 20 2012-10-20 36.0937500      0.00000
## 21 2012-10-21 30.6284722      0.00000
## 22 2012-10-22 46.7361111      0.00000
## 23 2012-10-23 30.9652778      0.00000
## 24 2012-10-24 29.0104167      0.00000
## 25 2012-10-25  8.6527778      0.00000
## 26 2012-10-26 23.5347222      0.00000
## 27 2012-10-27 35.1354167      0.00000
## 28 2012-10-28 39.7847222      0.00000
## 29 2012-10-29 17.4236111      0.00000
## 30 2012-10-30 34.0937500      0.00000
## 31 2012-10-31 53.5208333      0.00000
## 32 2012-11-01 37.3825996     34.11321
## 33 2012-11-02 36.8055556      0.00000
## 34 2012-11-03 36.7048611      0.00000
## 35 2012-11-04 37.3825996     34.11321
## 36 2012-11-05 36.2465278      0.00000
## 37 2012-11-06 28.9375000      0.00000
## 38 2012-11-07 44.7326389      0.00000
## 39 2012-11-08 11.1770833      0.00000
## 40 2012-11-09 37.3825996     34.11321
## 41 2012-11-10 37.3825996     34.11321
## 42 2012-11-11 43.7777778      0.00000
## 43 2012-11-12 37.3784722      0.00000
## 44 2012-11-13 25.4722222      0.00000
## 45 2012-11-14 37.3825996     34.11321
## 46 2012-11-15  0.1423611      0.00000
## 47 2012-11-16 18.8923611      0.00000
## 48 2012-11-17 49.7881944      0.00000
## 49 2012-11-18 52.4652778      0.00000
## 50 2012-11-19 30.6979167      0.00000
## 51 2012-11-20 15.5277778      0.00000
## 52 2012-11-21 44.3993056      0.00000
## 53 2012-11-22 70.9270833      0.00000
## 54 2012-11-23 73.5902778      0.00000
## 55 2012-11-24 50.2708333      0.00000
## 56 2012-11-25 41.0902778      0.00000
## 57 2012-11-26 38.7569444      0.00000
## 58 2012-11-27 47.3819444      0.00000
## 59 2012-11-28 35.3576389      0.00000
## 60 2012-11-29 24.4687500      0.00000
## 61 2012-11-30 37.3825996     34.11321
```

```r
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
```

In building this report, I iteratively explored the no missing and imputed data frames.

- The no missing has 53 observations (some dates missing from Oct 1 to Nov 30).
- The imputed has 61 observations which means that all the dates are covered.
- For the 53 dates that had data, the data is identical on the imputed data frame.
- 8 days have been imputed and they are identical (based on methodology).
- The Total steps over the period would increase due to the addition of 8 days.
- A final artifact is that there is now a median for these 8 days because there is no 0 observations. (weakness of simple imputation method).

### 3.8 Are there differences in activity patterns between weekdays and weekends? ###

```r
## Prior to calculating statistics and plotting - split data into weekday and weekend.
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

for (i in 1:nrow(df.statistics.steps.by.interval.imputed)) {

        if (weekdays(df.statistics.steps.by.interval.imputed[i,"ymd"]) %in% weekdays1) {
            df.statistics.steps.by.interval.imputed[i, "MF.or.SS"] <- 'weekday'
        } else {
            df.statistics.steps.by.interval.imputed[i, "MF.or.SS"] <- 'weekend'
        }
}

## Calculate Average (mean) steps by interval on an imputed data frame.
df.statistics.steps.by.interval.imputed <- summaryBy(steps.mean ~ interval + MF.or.SS,
                                                     data = df.statistics.steps.by.interval.imputed,
                                                     FUN = list(mean),
                                                     na.rm = TRUE)

## Creating panel plot to match assignment sample.
## I relabled intervals to 24 hour time to reframe observational differences.
attach(df.statistics.steps.by.interval.imputed)
xyplot(steps.mean.mean ~ interval | factor(MF.or.SS),
       type = 'l',layout = c(1,2),
       xlab = '24 Hour Time',ylab = 'Number of Steps',
       scales=list(x=list(labels=c("24:00","00:00","05:00","10:00","15:00","20:00","01:00"))))
```

![](PA1_template_files/figure-html/panel.plot-1.png)<!-- -->

Yes there are differences in activity patterns between weekdays and weekends. For weekdays and weekends there is little activity from midnight to 5:00 AM.  On weekdays there is much more activity from 5:00 AM until prior to 10:00 AM. The activity level is higher and longer on weekends from 10:00 AM until 21:00.  Activity tapers off around 20:00 on weekdays.  Both then have low activity until 5:00 AM the following day.
