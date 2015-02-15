# Reproducible Research: Peer Assessment 1
Prady Misra  
Sunday, February 15, 2015  


## Loading and preprocessing the data

The dataset is already available in the GitHub repository so there is no need to download. Unzipping the folder will give us the CSV file we need. The resulting file has the same name activity.csv as in the zipped folder. We also verify that a sub directory named **figure** exists in current working directory and create it if it does not.


```r
unzip ("activity.zip")

# All plots will be stored in "figure" directory, make sure it exists else create
if (!file.exists("figure")) 
    dir.create("figure")
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
f <- read.csv("activity.csv", stringsAsFactors=FALSE)
# Do some poking around to reconfirm what is already stated in assignment
str(f)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Here is what we find:
   1. dataset has entries for dates from Oct 1 thru Nov 30, 2012 means 61 days 
   2. measurements for each day are in 5 min intervals meaning 288 per day
   3. total number of observations are thus 288 * 61 = 17568
   4. several values for steps are missing

## What is mean total number of steps taken per day?

To calculate the total number of steps taken per day, we add up the steps for every day while ignoring the non-existent values (NA).


```r
# Ignore the NA values in the dataset, use built-in R features na.rm
i <- 1
totaldf <- data.frame()
while (i < nrow(f)) {
    last <- i+287   
    total <- sum(f[i:last,]$steps, na.rm=TRUE)
    temp <- data.frame(as.character(f$date[i]), total)
    totaldf <- rbind(totaldf, temp) 
    i <- last+1
}
colnames(totaldf) <- c("date", "totals")
png(file="figure/fig1.png")
hist(totaldf$totals, col="Red", main="Distribution of Total Steps Taken Everyday", 
                     xlab="Total Steps Per Day", ylab="Frequency", breaks=15,
                     xlim=c(0,25000), ylim=c(0,20))
dev.off()
```

```
## pdf 
##   2
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

#### Mean and Median of total number of steps taken everyday over 61 days

```r
paste("Mean of the total number of steps is", mean(totaldf$totals))
```

```
## [1] "Mean of the total number of steps is 9354.22950819672"
```

```r
paste("Median of the total number of steps is", median(totaldf$totals))
```

```
## [1] "Median of the total number of steps is 10395"
```


## What is the average daily activity pattern?

Calculate average number of steps taken, averaged across all days for each interval. Order the data frame by intervals then compute mean for subset containing one interval at a time for all intervals. Note that there are 61 rows for each interval and there are 288 intervals per day. Put results into a new data frame.


```r
f2 <- arrange(f, interval)
i <- 1
df2 <- data.frame()
while (i < nrow(f2)) {
  last <- i+60
  avgsteps <- mean(f2[i:last,]$steps, na.rm=TRUE)
  temp <- data.frame(f2$interval[i], avgsteps)
  df2 <- rbind(df2, temp) 
  i <- last+1
}
colnames(df2) <- c("interval", "avgsteps")
png(file="figure/fig2.png")
with (df2, plot(interval, avgsteps, type = "l",
      main="Avg Number of Steps Taken for Each interval \n (averaged across all days)", 
      xlab="Intervals", ylab="Avg Num of Steps", xlim=c(0,2500)))
dev.off()
```

```
## pdf 
##   2
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### The interval containing maximum number of steps

We can filter() the row which has the highest value of avgsteps to get the answer. Note that this number is corroborated by the plot too.


```r
library(dplyr, quietly=TRUE)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
paste("The interval containing maximum number of steps is", 
       (filter(df2, avgsteps==max(avgsteps)))$interval)
```

```
## [1] "The interval containing maximum number of steps is 835"
```


## Imputing missing values

Here is a brief step by step process:

   * Calculate and report total number of missing values (NA) in the dataset
   * Devise a strategy to fill in reasonable values for missing values.I decided to use the mean for that day as the value for missing steps data for that day.
   * Create a new dataset with missing values filled in
   * Make a hostogram of total # steps taken, calculate mean and median of totals

#### Total number of missing values in the original dataset

```r
paste("Total number of missing values in the original dataset is", sum(is.na(f$steps)))
```

```
## [1] "Total number of missing values in the original dataset is 2304"
```

#### Replacing NAs with mean for each interval

```r
f3 <- f2
i <- 1
for (k in 1:61)     # outer loop
    {
    for (j in 1:288)  #inner loop
      {
      if (is.na(f3$steps[i]))
          f3$steps[i] <- df2$avgsteps[j]
      i <- i+1
    }
}
```

#### Make a histogram of new totals

```r
f4 <- f3
f4$date <- as.POSIXct(f4$date)
f5 <- arrange(f4, date, interval)

i <- 1
newtotaldf <- data.frame()
while (i < nrow(f5)) {
  last <- i+287
  total <- sum(f5[i:last,]$steps, na.rm=TRUE)
  temp <- data.frame(as.character(f5$date[i]), total)
  newtotaldf <- rbind(newtotaldf, temp) 
  i <- last+1
}
colnames(newtotaldf) <- c("date", "totals")
png(file="figure/fig3.png")
hist(newtotaldf$totals, col="red", 
    main="Distribution of Total Steps Taken Everyday \n (New Dataset with NAs filled in)", 
    xlab="Total Steps Per Day", ylab="Frequency", breaks=15, ylim=c(0,20))
dev.off()
```

```
## pdf 
##   2
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
  
#### Mean and Median of total number of steps taken everyday for the updated dataset

```r
paste("Mean of the total number of steps is", mean(newtotaldf$totals))
```

```
## [1] "Mean of the total number of steps is 10766.1886792453"
```

```r
paste("Median of the total number of steps is", median(newtotaldf$totals))
```

```
## [1] "Median of the total number of steps is 10766.1886792453"
```


## Are there differences in activity patterns between weekdays and weekends?

Here is how we approach this:
   * Create a factor variable in dataset with 2 values - weekday and weekend
   * Make plots for avg steps taken across all intervals on weekdays and weekends


```r
# Define a function that given a date returns if it is a weekday (M-F) or Weekend (S, SA)
dayname <- function (date) 
    {
    if (weekdays(date) == "Sunday" | weekdays(date) == "Saturday")"Weekend" else "Weekday"
}
```

For each date in the date column of updated dataset determine if it is a Weekday (M-F) or a Weekend (Sat or Sun). Add a column (factor variable) indicating it.


```r
f6 <- f5
wdf <- data.frame()
i <- 1
while (i <= nrow(f6))
  {
  tmp <- data.frame(as.factor(dayname(f6$date[i])))
  wdf <- rbind(wdf, tmp)
  i <- i+1
}
colnames(wdf) <- c("days")
fnew <- cbind(f6, wdf)
rm(wdf)
```

We subset the dataframe for only weekdays, find total number of rows and use that to determine number of rows for an interval over which we need to calculate the average. We know that each day has 288 of 5 minute intervals. So the number of rows for each interval will be total_rows/288 i.e. nrow(fwkday)/288. Same process is followed for weekend subset.


```r
fnew1 <- ddply(fnew, c("interval", "days"), summarise, avgsteps = mean(steps))
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```r
png(file="figure/fig4.png")
g <- ggplot(fnew1, aes(interval, avgsteps, ))
g <- g + geom_line(aes(color=days)) + facet_grid (. ~days)
g <- g + labs (title = "Steps Taken for Each Interval\n(averaged across weekdays & Weekends)")
g <- g + labs ( x = "Intervals", y = "Avg Num of Steps")
print(g)
dev.off()
```

```
## pdf 
##   2
```


![](./PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

