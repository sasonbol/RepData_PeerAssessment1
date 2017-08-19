#loading data
data<-read.csv("activity.csv")
#getting the total number of steps per day, their mean & median
#dplyr package is installed and loaded to be used
install.packages("dplyr")
library(dplyr)
#to get the number of total steps each day with exclusion of missing values
d<-filter(data, !is.na(steps))%>%
        group_by(date)%>%
        summarize(total.steps=sum(steps))
#calcualte the mean and median of the total number of steps taken per day
mean(d$total.steps)
median(d$total.steps)
#Histogram of the total number of steps taken each day
hist(d$total.steps, col="blue", main="Histogram of the total number of steps taken each day",
             xlab="steps/day", ylab="frequency")

#getting the average daily activity pattern
d2<-filter(data, !is.na(steps))%>%
        group_by(interval)%>%
        summarize(total.steps= sum(steps),avg.steps=mean(steps))
print(d2)#just to check that everything is OK
#plot
plot(d2$interval, d2$avg.steps, type="l",
     main="Time series plot of the average number of steps taken",
        xlab="intervals min", ylab="average number of steps across all days")
#to get the maximum average of steps
max(d2$avg.steps)
#to determine to which interval it belongs
subset(d2$interval,d2$avg.steps== max(d2$avg.steps))

#to detrmine the total number of missing values in the original dataframe(data)
sum(is.na(data$steps))
#steps column was chosen as all missing values were in this column
#However, using the whole dataframe will render the same value
#imputing missing values in a new datframe
data2<-data
#replacing missing values with the average number of steps for each interval
data2$steps[is.na(data2$steps)]<-d2$avg.steps[match(data2$interval,d2$interval)]
#this will produce a warning message which can be ignored without affecting the results
#to check that missing values were correctly replaced
head(data2)
d3<- group_by(data2,interval)%>%
        summarize(total.steps= sum(steps),avg.steps=mean(steps))
print(d3)
#we can observe that the total number of steps has increased for each interval 
#without affecting te average number of steps

#getting the total number of steps per day,after imputing missing values
d4<-group_by(data2,date)%>%
        summarize(total.steps=sum(steps))
#Histogram of the total number of steps taken each day after imputing missing values
hist(d$total.steps, col="red", main="Histogram of the total number of steps 
             taken each day after imputing missing values",
             xlab="steps/day", ylab="frequency")

#to check the pattern of average steps between weekdays and weekends
#first check the class of the date variable in data2
class(data2$date)
# as the class appears to be a factor, it must be converted into POSIXct
data2$date<-as.POSIXct(data2$date)
#to aid in classifying the weekdays into weekend and weekday, a vector named
#weekend.vec was created
weekend.vec<-c("Saturday", "Sunday")
#using the dplyr package on data2, a new column  will be created which is a factor 
#with two levels "weekday"& "weekend"
data2<-mutate(data2,wk.day=factor((weekdays(data2$date) %in% weekend.vec), 
                                  levels=c(TRUE, FALSE), labels=c("weekend", "weekday")))
#to check the presence of our new column with its correct information
head(data2)
data2[4030:4050,]
#to check the class of this variable
class(data2$wk.day)
levels(data2$wk.day)

#To get the averge number of steps at each interval in weekends & weakdays
#data2 was grouped according to both variables, then the mean was calculated
#using dplyr package

d5<-group_by(data2,wk.day,interval)%>%
        summarize(average.steps=mean(steps))
print(d5)
#to compare the relationship between  the 5-minute-intervals and the average number of 
#steps in weekdays & weekends, a time-series plot using lattice package was plotted
install.packages("lattice")
library(lattice)
xyplot(average.steps~interval|wk.day, data=d5, xlab= "interval", 
       ylab="Average number of steps", layout=c(1,2), type="l" )
