# test
SCarter  
January 24, 2016  

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

You can also embed plots, for example:

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

## Set work directory
path_mydir = "C:/Users/Suzanne/Documents/AAAAAA_SUZ FOLDERS/CAREER/suz_Data Science Coursera/Reproducible Research/RepData_PeerAssessment1"
setwd(path_mydir)


#bring packages into session.

install.packages("ggplot2")
install.packages("knitr")      
install.packages("scales")      
install.packages("ggthemes", dependencies = TRUE)
install.packages(plyr)

library(ggplot2)
library(knitr)
library(scales)
library(ggthemes)
library(plyr)


#unzip and read the provided dataset:

data <- read.csv(unz("activity.zip", "activity.csv"), colClasses = c("numeric", "Date", "numeric"))

#Research Question:  What is mean total number of steps taken per day?
#For this question, missing values in the dataset are ignored.

#Calculate total number of steps per day.
daily <- aggregate(steps ~ date, data=data, FUN=sum)

#Create histogram of total number of steps per day.
hist(daily$steps, border = "blue", col = "white", ylim=c(0,50), main="Histogram of total number of steps/day",     
     xlab="Total number of steps/day")     

#Calculate mean and median number of steps per day.
mean(daily$steps)
median(daily$steps)

# mean(daily$steps)
#  [1] 10766.19
# median(daily$steps)
#  [1] 10765

#Research Question:  What is the average daily activity pattern?

#Create time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


intervalsteps = ddply(data, .(interval), summarize, 
                      meansteps = mean(steps, na.rm = TRUE))
ggplot( intervalsteps, aes(x=interval, y=meansteps)) + geom_line() +
  theme_few() + 
  guides(fill=FALSE)+
  xlab( "Interval") + 
  ylab( "Average Number of Steps") +
  ggtitle( "Average Number of Steps by Time of Day") + 
  theme(plot.title = element_text(size = rel(1.75)))

intervalmax = intervalsteps$interval[which.max(intervalsteps$meansteps)]  
intervalmax

# intervalmax
#  [1] 835    


#Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#Create boolean array to identify NAs.  
#Total up the number of TRUEs (Identified NAs).

na_ID = is.na(data$steps)
sum(na_ID)


#Replace NA with mean of 5-minute interval (already computed).  

sapply(split(data, data$date), function(x) sum(is.na(x$steps)))

#Create a new dataset that is equal to the original dataset (data) but with the missing data filled in (imputed).

imputed <- data
imputed[na_ID, "steps"] <- rep(daily2$steps, length.out = nrow(data))[na_ID]

#Calculate total number of steps per day.
daily_imputed <- aggregate(steps ~ date, data=imputed, FUN=sum)

#Create histogram of total number of steps per day.
hist(daily$steps, border = "blue", col = "white", ylim=c(0,50), main="Histogram of total number of steps/day - imputed data",     
     xlab="Total number of steps/day - imputed data")     

#Calculate mean and median number of steps per day.
mean(daily$steps)
median(daily$steps)

#Calculate and report the mean and median total number of steps taken per day. 

#mean(daily$steps)
#[1] 10766.19
# median(daily$steps)
#[1] 10765


#Do these values differ from the estimates from the first part of the assignment? 
#No.

#What is the impact of imputing missing data on the estimates of the total daily number of steps?

#The chosen method of imputation was mean of 5-minute interval, so it makes sense that:
#1. the overall mean of the "imputed" data would not change
#2. the MEDIAN value (the "middle" value) would get closer to the mean

#Research Question:  Are there differences in activity patterns between weekdays and weekends?
#Imputed dataset to be used.

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend". 
#This factor indicates whether a given date is a weekday or weekend day.

imputed$day = weekdays( as.Date(imputed$date, "%Y-%m-%d") )
weekend.values = c("Saturday", "Sunday")
weekend.flag = imputed$day %in% weekend.values
imputed$day[ weekend.flag ] = "Weekend"
imputed$day[ !(weekend.flag) ] = "Weekday"
imputed$day = as.factor(imputed$day)
stepsbyday = ddply(imputed, .(day, interval), summarize, 
                   meansteps = mean(steps, na.rm = TRUE))

#Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


ggplot( stepsbyday, aes(x=interval, y=meansteps)) + geom_line() +
  theme_few() + 
  guides(fill=FALSE)+
  facet_grid(day ~ .) + ylab("Number of Steps") +
  xlab("Interval") +
  ggtitle("Imputed Levels: Weekends vs. Weekdays") + 
  theme(plot.title = element_text(size = rel(1.75)))

#Conclusion based on plots: Yes there are differences in activity patterns.

#On weekdays, there is more concentrated activity, as shown by two aspects of the chart:
#high peak of over 200 steps at interval 835, while at other times of the day, steps taken range from zero to just over 100.  

#On weekends, there is more activity througout the day, 
#as evidenced by several peaks at around 150 steps taken.   
