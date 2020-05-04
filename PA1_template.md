---
title: "RepData - PeerAssessment 1"
output: 
  html_document:
    df_print: paged
---

It is the Project 01 of the Reproducible Research course from Coursera. 

# Loading and preprocessing the data


**1. Load the data (i.e. read.csv())**


**2 . Process/transform the data (if necessary) into a format suitable for your analysis**


```{r}

# load the packages
library("tidyverse")
#library("dplyr")
library("data.table")
#library("ggplot2")

#path <- getwd()
#download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"              , destfile = paste(path, "rep_data_activity.zip", sep = "/"))

#unzip(zipfile = "rep_data_activity.zip")

ACT <- read_csv("activity.csv")
glimpse(ACT)

```

```{r}

## Observations: 17,568
## Variables: 3
## $ steps    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ date     <date> 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012...
## $ interval <dbl> 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 10...
```

# What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

**1. Make a histogram of the total number of steps taken each day**


Remove the NaN values

```{r}

ACT_clean <- ACT %>% drop_na()

```



Number of the steps per day and the histogram

```{r}
steps_per_day <- ACT_clean %>%
  group_by(date) %>%
  summarize(sum_steps = sum(steps))

ggplot(steps_per_day, aes(x = date
                          , y = sum_steps)) +
  geom_bar(stat = "identity", fill ="#FF9919", width=0.75) +
  ggtitle("Histogram of the total number of steps taken each day") +
  xlab("Date") +
  ylab("Total number of steps") 

```


![](https://github.com/alambike123/RepData_PeerAssessment1/blob/master/01.png)<!-- -->



**2 . Calculate and report the mean and median total number of steps taken per day**


```{r}

steps_per_day %>%
  summarize(steps_mean = round(mean(sum_steps)), steps_median = median(sum_steps))

```

| steps_mean | steps_median |
|------------|--------------|
| 10766      | 10765        |


# What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```{r}

interval <- ACT_clean %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps))

plot(x=interval$interval, y=interval$mean_steps, type = 'l'
     , xlab = 'Interval', ylab = 'Avg. Steps per day'
     , main = 'Time series plot - Avg. Daily Steps' )

```

![](https://github.com/alambike123/RepData_PeerAssessment1/blob/master/02.png)

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```{r}
max <- which(interval$mean_steps == max(interval$mean_steps))
max_interval <- ACT_clean$interval[max]
max_interval
```

```{r}
## [1] 835


```

# Imputing missing values

- Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

Check the total numbers of NaN values.

```{r}
sum(is.na(ACT))
```

```{r}
## [1] 2304


```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**

- Using the steps mean to substitute the NaN values for each interval 

```{r}

#ACT %>% replace(is.na(.), mean(ACT$steps, na.rm =  TRUE))

ACT_clean <- ACT %>% drop_na()


interval <- ACT_clean %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ACT_interval <- ACT

#interval <- aggregate(steps ~ interval, data = ACT, mean, na.rm = TRUE)

for (i in 1:nrow(ACT_interval)){
  if (is.na(ACT_interval$steps[i])){
    ACT_interval$steps[i] <- interval$steps[which(ACT_interval$interval[i] == interval$interval)]}
}


```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r}
data.table::fwrite(x = ACT_interval, file = "tidyData.csv", quote = FALSE, )

```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
steps_Nan <- ACT_interval %>%
  group_by(date) %>%
  summarize(sum_steps = sum(steps))

ggplot(steps_Nan, aes(x = date
                          , y = sum_steps)) +
  geom_bar(stat = "identity", fill ="#FF9919", width=0.75) +
  ggtitle("Histogram of the total number of steps taken each day") +
  xlab("Date") +
  ylab("Total number of steps") 

```

![](https://github.com/alambike123/RepData_PeerAssessment1/blob/master/03.png)

```{r}

steps_Nan %>%
  summarize(steps_mean = round(mean(sum_steps)), steps_median = median(sum_steps))

```

| steps_mean | steps_median |
|------------|--------------|
| 10766      | 10766.19     |

```{r}
Impactdiff <- (sum(ACT_interval$steps) - sum(ACT_clean$steps)) / sum(ACT_clean$steps) 
Impactdiff

```

```{r}
## [1] 0.1509434

```

- It can be seen a difference of about 15% between datasets with the missing data and without missing data.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). **

```{r}


new_ACT <- read_csv("tidyData.csv")

library("plyr")
library("lattice")
Sys.setlocale("LC_TIME", "English") 
```

```{r}
## [1] "English_United States.1252"
```


```{r}

new_ACT$weekdays <- weekdays(as.Date(new_ACT$date))
new_ACT$weekdays <- ifelse(new_ACT$weekdays %in% c("Saturday", "Sunday"),"weekend", "weekday")  
average <- ddply(new_ACT, .(interval, weekdays), summarise, steps=mean(steps))
xyplot(steps ~ interval | weekdays, data = average, layout = c(1, 2), type="l", xlab = "Interval", ylab = "Number of steps")


```
![](https://github.com/alambike123/RepData_PeerAssessment1/blob/master/04.png)

