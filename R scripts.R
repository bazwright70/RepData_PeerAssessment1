# load libraries
install.packages(c("mice",
                   "readr",
                   "ggplot2",
                   "dplyr"))
library(readr)
library(ggplot2)
library(dplyr)
library(knitr)


# get data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "./zipfile")
act.data <- read_csv(unzip( "./zipfile"))

# study data
head(act.data,10)
summary(act.data)

# create summary table
act.by.day <- act.data %>%
    select(steps,date) %>%
    group_by(date) %>%
    summarise(steps = sum(steps))

# summary of data, mean/median steps per day
mean(act.by.day$steps, na.rm = TRUE)
median(act.by.day$steps,na.rm = TRUE)

# histogram of steps / day
ggplot(data  =  act.data, aes(x = (steps)))+
    geom_histogram(bins = 30, col = "black",
                   fill = "red")+
    scale_x_continuous(breaks = seq(0,800,50))+
    scale_y_log10()+
    labs(x = "Number of Steps", y = "Count (Log10)")

# plot of average steps per day
ggplot(data = act.by.day, aes(x = date,y = steps))+
    stat_summary(geom = "line", fun.y = mean,col = "red")+
    labs(x = "Date", y = "Average Steps per Day")

# 5 minute interval with the most steps
act.data %>%
    filter(!is.na(steps)) %>%
    summarise(Interval = interval[which(steps == max(steps))],
              Steps = max(steps)) 

# count number of days with missing step values
sum(is.na(act.data$steps))

# fill missing data with imputed values
act.data.imp <- act.data

set.seed(2111)
act.data.imp$steps  <- sapply(act.data$steps, function(x){
    if(is.na(x)){
        x <- round(mean(act.data$steps,na.rm = T) + runif(1,-30,30))
    } else x <- x
})
 
# create dataframe of steps per day including imputed values
act.by.day.imp <- act.data.imp %>%
    select(steps,date) %>%
    group_by(date) %>%
    summarise(steps = sum(steps))

# create histogram of average steps per day
ggplot(data = act.by.day.imp, aes(x = date,y = steps))+
    stat_summary(geom = "line", fun.y = mean,col = "red")+
    labs(x = "Date", y = "Average Steps per Day")

# mean and median of imputed data
mean(act.by.day.imp$steps)
median(act.by.day.imp$steps)

# add factor to imputed dataset for weekday or weekend

act.data.imp$day.type <- weekdays(act.data.imp$date)
act.data.imp$day.type <- sapply(act.data.imp$day.type,
                function(x){
                    ifelse(x %in% c("Saturday","Sunday"),
                        "Weekend","Weekday")          
                })
act.data.imp$day.type <- as.factor(act.data.imp$day.type)

