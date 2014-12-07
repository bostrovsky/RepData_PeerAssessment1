
## Dataset: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

## steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
## date: The date on which the measurement was taken in YYYY-MM-DD format
## interval: Identifier for the 5-minute interval in which measurement was taken

setwd("~/Desktop/Reproducable Research")
activity <- read.csv("~/Desktop/Reproducable Research/activity.csv")

activity$date <- as.Date(activity$date, "%Y-%m-%d")

## Create a Histogram of the number of steps per day
totalSteps  <- aggregate(steps~date, data = activity, sum, na.rm = TRUE)
par(mar=c(6,9,4,1))
par(cex=.5)
histogram<-barplot(totalSteps$steps, names.arg = totalSteps$date,ylab="          Steps", main="Number of Steps Per Day", col="red")

##Calculate Mean and Median Number of Steps
meanSteps  <-  mean(totalSteps$steps)
medianSteps <-  median(totalSteps$steps)

## Average number of steps per interval
aveInterval <- aggregate(steps ~ interval, activity, mean)
plot(aveInterval, type="l", xlab="Interval", ylab="Average Steps", main="Average Steps Per 5 minute Interval")

aveInterval$interval[which.max(aveInterval$steps)]
## How many cases are incomplete?
sum(!complete.cases(activity))

fillInNAs <- numeric()
for (i in 1:nrow(activity)) {
        observation <- activity[i, ]
        if (is.na(observation$steps)) {
                steps <- subset(aveInterval, interval == observation$interval)$steps
        } else {
                steps <- observation$steps
        }
        fillInNAs <- c(fillInNAs, steps)
}

filledActivity <- activity
filledActivity$steps <- fillInNAs

## Create another Histogram of the number of steps per day with the dataset after filling in the NAs
totalStepsNoNA  <- aggregate(steps~date, data = filledActivity, sum, na.rm = TRUE)
par(mar=c(6,9,4,1))
par(cex=.5)
histogram<-barplot(totalStepsNoNA$steps, names.arg = totalStepsNoNA$date,ylab="          Steps", main="Number of Steps Per Day", col="blue")

##Calculate Mean and Median Number of Steps
meanStepsNoNA  <-  mean(totalStepsNoNA$steps)
medianStepsNoNA <-  as.integer(median(totalStepsNoNA$steps))


filledActivity$weekday  <- weekdays(filledActivity$date)

for (i in 1:nrow(filledActivity)) {
        if (filledActivity$weekday[i] == "Saturday" | filledActivity$weekday[i] == "Sunday") {
                filledActivity$weekDayOrEnd[i]  <- "weekend" }    
        else {
                filledActivity$weekDayOrEnd[i]  <- "weekday"}
}

aveIntervalF <- aggregate(steps ~ interval + weekDayOrEnd, data = filledActivity, mean)
names(aveIntervalF) <- c("interval", "weekDayOrEnd", "steps")
library(lattice)
xyplot(steps ~ interval | weekDayOrEnd, aveIntervalF, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")






