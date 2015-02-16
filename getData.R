## load the dataset
## set global variables 
setglobalVariable <- function(){
        activity<<-NULL
        ## setting global variables to null
        
}   

readFile<- function(fname){
        ## read the csv file that contains headers
        ## any blanks as NA, turn off interpretation of comments
        ## do i need to specify data types for date and interval columns?
        ## , colClasses=c("integer", "character", "character") 
        x<-read.csv(fname, header = TRUE)
        x$date<-ymd(x$date)
        activity<<-x
       
        
}

getfile<- function(){
                ## zipped file in repository
                ## unzip and send to readFile function
                temp <- "repdata-data-activity.zip"
                ## Create a temp file to hold the Zipped file
                temp<-unzip(temp,files="activity.csv")
                ## unzip the temp file to activity.csv
                readFile(temp)
                ## read temp
                unlink(temp)
                
}
## check if global variable is null, if it is retrieve file
if(exists("activity", mode = "any")){
        if(is.null(activity)){ 
                getfile()
        }
} else {
        setglobalVariable() 
        getfile()
}


## Make a histogram of the total number of steps taken each day
y<- ddply(activity, c("date"), summarize, steps=sum(steps))
png(filename = "plot1.png", width = 480, height = 480, units = "px", bg = "transparent")
hist(y$steps, xlab="Steps", ylab="daily", main="Steps per day", col="red")


## What is mean total number of steps taken per day?
mean(y$steps, na.rm=TRUE)

## Median total number of steps taken per day
median(y$steps, na.rm=TRUE)


## What is the average daily activity pattern?

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
z<-ddply(activity, c("interval"), summarize, steps=sum(steps, na.rm=TRUE))
plot(z$interval, z$steps, type='l', xlab="Interval", ylab="steps")

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
subset(z,z$steps==max(z$steps), select = interval)


## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
count(activity[!complete.cases(activity),])

## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use 
## the mean/median for that day, or the mean for that 5-minute interval, etc.
## remove the missing values from the dataset
## x$steps[is.na(x$steps)] <- 0


## Create a new dataset that is equal to the original dataset but with the missing data filled in.

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends?
