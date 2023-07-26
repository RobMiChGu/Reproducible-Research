---
title: "PA1_template"
author: "Michael Guril"
date: "2023-07-26"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Libraries
```{r}
library(lattice)
library(dplyr)
library(ggplot2)
```


#Code for reading in the dataset and/or processing the data
```{r}
activity <- read.csv("D:/Data Science Foundations using R/5 Reproducible Research/Woche 2/Course Project 1/activity.csv")
```


#Datas
```{r}
str(activity)
summary(activity)
head(activity)
```


#Histogram of the total number of steps taken each day
```{r}
totalStepsByDay<-aggregate(steps~date, activity, sum)
hist(totalStepsByDay$steps, xlab="total number of steps per day", 
     ylab="number of days", main="Histogram of the total number of steps taken each day")
```


#Mean and median number of steps taken each day
```{r}
mean_activity<-mean(totalStepsByDay$steps)
mean_activity
```

```{r}
median_activity<-median(totalStepsByDay$steps)
median_activity
```


#Time series plot of the average number of steps taken
```{r}
averageStepsbyInterval<-aggregate(steps~interval, activity, mean)
with(averageStepsbyInterval, plot(interval, steps, type = "l"))
```


#The 5-minute interval that, on average, contains the maximum number of steps
```{r}
averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
```


#Code to describe and show a strategy for imputing missing data
```{r}
missingIndex<-is.na(activity[,1])
activitym<-mean(averageStepsbyInterval$steps)
activityNEW<-activity
activityNEW[missingIndex,1]<-activitym
```


#Histogram of the total number of steps taken each day after missing values are imputed
```{r}
totalStepsByDayNEW<-aggregate(steps~date, activityNEW, sum)
hist(totalStepsByDayNEW$steps, xlab="Class of Total Number of Steps per day", 
     ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
```


#Mean and median number of steps taken each day (NEW)
```{r}
totalStepsByDayNEW<-aggregate(steps~date, activityNEW, sum)
mean_activity_afterImput<-mean(totalStepsByDayNEW$steps)
mean_activity_afterImput
```

```{r}
median_activity_afterImput<-median(totalStepsByDayNEW$steps)
median_activity_afterImput
```


#Panel plot average number of steps taken per 5-minute interval weekdays
```{r}
activityNEW$date<-as.Date(activityNEW$date)

activityfinal<-activityNEW %>%
    mutate(dayType= ifelse(weekdays(activityNEW$date)=="Saturday" | weekdays(activityNEW$date)=="Sunday", "Weekend", "Weekday"))

averageStepByDayTypeAndInterval<-activityfinal %>%
    group_by(dayType, interval) %>%
    summarize(averageStepByDay=sum(steps))

with(averageStepByDayTypeAndInterval,
    xyplot(averageStepByDay ~ interval | dayType, type = "l", 
       main = "total number of steps within intervals by daytype", 
       xlab = "daily intervals", 
       ylab = "average number of steps"))
```

