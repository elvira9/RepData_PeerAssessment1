# Reproducible Research: Peer Assessment 1
María Elvira Ferre Jaén (mariaelvira.ferre@um.es)  
August 12th, 2015   



\ 

### Loading and processing the data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

__Exploration of the data and data structure.__  


```r
# download data from the source url
fpath <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file( fpath, destfile = paste( getwd(), "/", "activity.zip", sep = "" ), method = "wget" )           
unzip( "activity.zip" )

# Load the data
df <- read.table( "activity.csv", sep = ",", header = TRUE )

# Show the data in a html table 
sum <- summary( df )
xtab <- xtable( sum )
print( xtab, type = "html" )
```

<!-- html table generated in R 3.0.2 by xtable 1.7-4 package -->
<!-- Wed Aug 12 20:33:02 2015 -->
<table border=1>
<tr> <th>  </th> <th>     steps </th> <th>         date </th> <th>    interval </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> Min.   :  0   </td> <td> 2012-10-01:  288   </td> <td> Min.   :   0   </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 1st Qu.:  0   </td> <td> 2012-10-02:  288   </td> <td> 1st Qu.: 589   </td> </tr>
  <tr> <td align="right"> 3 </td> <td> Median :  0   </td> <td> 2012-10-03:  288   </td> <td> Median :1178   </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Mean   : 37   </td> <td> 2012-10-04:  288   </td> <td> Mean   :1178   </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 3rd Qu.: 12   </td> <td> 2012-10-05:  288   </td> <td> 3rd Qu.:1766   </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Max.   :806   </td> <td> 2012-10-06:  288   </td> <td> Max.   :2355   </td> </tr>
  <tr> <td align="right"> 7 </td> <td> NA's   :2304   </td> <td> (Other)   :15840   </td> <td>  </td> </tr>
   </table>


\ 
The variables included in this dataset are:

* __steps__: Number of steps taking in a 5-minute interval (missing values are coded as  NA )
* __date__: The date on which the measurement was taken in YYYY-MM-DD format
* __interval__: Identifier for the 5?minute interval in which measurement was taken

The dataset is stored in a CSV file and there are a total of 17,568 observations in this dataset. It has arround 288 observations per day.

### What is mean total number of steps taken per day?


```r
# I ignore the missing values
dfm <- na.omit( df )
totStep <- tapply( dfm$steps, dfm$date, sum )
totStep <- as.numeric( totStep )
```

Histogram of total number of steps per day


```r
hist( totStep, main = "Histogram of the total steps by day",
     breaks = 50,
     freq = TRUE,
     col = "blue",
     xlab = "steps")
```

<img src="./PA1_template_files/figure-html/hist-stepByDay-1.png" title="" alt="" style="display: block; margin: auto;" />

The mean and median total number of steps taken per day



The mean of the total step per day is **10766.189** and the median is **10765**.

### What is the average daily activity pattern?

Below therea are a time series plot where we can see the average number of steps taken, averaged across all days.


```r
avdAct <- tapply( dfm$steps, dfm$interval, mean )
plot( avdAct, type = "l", xlab = "interval", ylab = "Average of steps", col ="purple" )
```

<img src="./PA1_template_files/figure-html/timeserie1-1.png" title="" alt="" style="display: block; margin: auto;" />


```r
maxInterv <- dimnames( avdAct )[[1]] [ rev( order( avdAct ) )[1] ]
maxi1 <- max( avdAct )
```

The 5 minute interval that on average across all the days in the dataset  contains the maximum number of steps is the **835**, the maximun is 206.17.

### Imputing missing values

The total number of missing values in the dataset is:


```r
nna <- as.numeric( sum( is.na( df$steps ) ) )
nna
```

[1] 2304


We will fill all of the missing values in the dataset with the mean for that 5-minute interval. And create a new dataset with the missing data filled in.   


```r
dft <- df  # with NAs
 for (i in 1:nrow(df)){     # We fill the dataframe with the mean and we create 'dft' 
     if ( is.na( df$steps[i] ) ){
         media <- mean( df$steps[ which( df$interval == df$interval[i] ) ], na.rm = TRUE )
         if (!is.na(media)) {
             dft$steps[i]<-media
          } else { dft$steps[i] <- 139 }         
        }     
 }
# dft is the new dataset with missing filled in
```

The number of missing data is 2304


```r
sumt <- tapply( dft$steps, dft$date, sum )
sumt <- as.numeric( sumt )
```

Histogram of total number of steps per day of the dataset without missig data


```r
hist( sumt, main = "Histogram of the total steps by day \n(no missig data)",
     breaks = 50,
     freq = TRUE,
     col = "green",
     xlab = "steps" )
```

<img src="./PA1_template_files/figure-html/hist-totalStepDay-1.png" title="" alt="" style="display: block; margin: auto;" />





After filling in the missing data with the mean of the interval, the mean of the total step per day is **10766.189** and the median is **10766.189**.   So we found very small diferences, oly the median change a bit.

### Are there differences in activity patterns between weekdays and weekends?

We create a new factor variable with two levels – "weekday" and "weekend".


```r
# change factor to dates and set if they are weekdays or weekends
dd <- as.character( dft$date )
dd <- as.Date( dd, "%Y-%m-%d" )
dw <- weekdays( dd)  # "sábado", "domingo"--> Saturday and Sunday
# table(dw)
dw [ dw == "sábado" | dw == "domingo" ] <-"weekend"
dw [ dw!= "weekend"] <-"weekday"
dft <- data.frame( dft, dw = factor( dw ) )
```


There is a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days:


```r
dfwdays <- dft[ dft$dw == "weekday", ] 
mwdays <- tapply( dfwdays$steps, dfwdays$interval, mean )
plot( mwdays, type = "l",
     xlab = "interval",
     ylab = "Average of steps",
     col = "blue",
     xlim = c( 0, 300 ),
     lwd = 1.5,
     asp = TRUE )

dfwend <- dft[ dft$dw == "weekend", ]
mwend <- tapply( dfwend$steps, dfwend$interval, mean )

# Plot
par( new = TRUE )
plot(mwend,
     type = "l",
     col = "red", 
     ylab ="", xlab = "",
     xaxt='n', yaxt = 'n',
     xlim = c( 0, 300 ),
     lwd = 1.5 )

legend( "topleft", inset=.05,
       c( "Weekdays", "Weekends" ),
       lty = c( 1,1 ),
       lwd = c( 1.5 , 1.5 ),
       col = c( "blue","red" ),
       horiz = FALSE )
```

<img src="./PA1_template_files/figure-html/activ-wday&wend-1.png" title="" alt="" style="display: block; margin: auto;" />
We observe that the activity on weekends tends to be more spread out over the day compared to the weekdays.

It seems that **on weekend the activity (steps) are extended over the intervals (day time) that on weekdays**. It could be because of the diferent day routines.


```r
par( mfrow = c( 2,1 ) )

plot( mwdays, type = "l",
     main = "Weekdays",
     xlab = "interval",
     ylab = " Average of steps",
     col = "blue",
     lwd = 1.5,
     xlim = c( 0, 300 )
     )

plot(mwend,
     type = "l",
     main = "Weekends",
     xlab = "interval",
     ylab = "average of steps",
     col = "red",
     xlim = c( 0, 300 ),
     lwd = 1.5 )
```

<img src="./PA1_template_files/figure-html/activ-wday&wend2-1.png" title="" alt="" style="display: block; margin: auto;" />

Finally, there is the infomation about my session.

```r
sessionInfo()
```

R version 3.0.2 (2013-09-25)
Platform: i686-pc-linux-gnu (32-bit)

locale:
 [1] LC_CTYPE=es_ES.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=es_ES.UTF-8        LC_COLLATE=es_ES.UTF-8    
 [5] LC_MONETARY=es_ES.UTF-8    LC_MESSAGES=es_ES.UTF-8   
 [7] LC_PAPER=es_ES.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=es_ES.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] lattice_0.20-29 xtable_1.7-4    knitr_1.8      

loaded via a namespace (and not attached):
 [1] codetools_0.2-8 digest_0.6.4    evaluate_0.5.5  formatR_1.0    
 [5] grid_3.0.2      htmltools_0.2.6 rmarkdown_0.3.3 stringr_0.6.2  
 [9] tools_3.0.2     yaml_2.1.13    

















