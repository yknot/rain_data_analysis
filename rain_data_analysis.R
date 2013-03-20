# code to analyze rain data from bocas del toro field station
#
# Andrew Yale
#
# used for EWB-RPI in their Panama Project
library("plotrix")

# read in data
# data in format of year,month,day,rain
rain.data <- read.csv("~/R/rain_data_analysis/rain_days_split2.csv",
                      header = T)

# simple boxplot of rain seperated into months
boxplot(rain.data$rain ~ rain.data$month,
        main = "Boxplot of Daily Rainfall by Month")

# find number of years in data
# use this rather then hard coded for reusabilty
years.vec <- unique(rain.data$year)
num.years <- length(unique(rain.data$year))

# monthly means
# set up with 12 values for each year
month.means <- NULL

MonthMean <- function(tmp.year, tmp.means){
  # Computes the mean rainfall for a month given a year and month
  #
  # Args:
  # tmp.year: year in which you want data from
  # tmp.means: temp vector to store the means in
  #
  # Returns:
  # the temp vector used to store means
  
  for(j in 1:12){
    tmp.means[j] <- mean(rain.data$rain
                                    [rain.data$year == tmp.year &
                                     rain.data$month == j],
                                    na.rm = TRUE)
  }
  return(tmp.means)
}

# counter to go throuh the month.means vector
count <- 0

# iterate through each year in data and call function to get each month mean
for(i in 1:num.years){
  tmp.means <- rep(NA,12)
  # call function to get month means for the year
  tmp.means <- MonthMean(years.vec[i], tmp.means)
  # store the temp means before they get put back to NA
  for(k in 1:12){
    count <- count + 1
    # store in each value of month means using counter to not overlap
    month.means[count] <- tmp.means[k]
  }
}


# run length encoding
rain.rle = rle(rain.data$rain)
# max days without rain
cat("Max days without rain =",
    max(rain.rle$lengths[rain.rle$values == 0]), "\n")
# mean interval of days without rain
cat("Mean days without rain =",
    mean(rain.rle$lengths[rain.rle$values == 0]), "\n")
# time series plot of rle lengths
ts.plot(rain.rle$lengths[rain.rle$values == 0],
        gpars = list(xlab = "Time",
          ylab = "Lengths of gaps", main = "Plot of gaps between rain"))

# scatterplot rain by rle
# plot.default(rain.data$rain, rle$lengths)

# time series analysis of month means
rain.data.ts <- ts(month.means, frequency = 12, start = c(2002, 6))
ts.plot(rain.data.ts, gpars = list(xlab = "Years", ylab = "Mean rain per month",
                        main = "Time Series Plot of Daily Rain Means by Month"))

# calculate conf intervals for daily rain data by month (unified over years)
# create empty vectors to store data
ci.months.lower <- rep(NA,12)
ci.months.upper <- rep(NA,12)
month.means.total <- rep(NA,12)
count <- 0

for(j in 1:12){
  # use t.test to calc conf ints
  # use try because of empty months
  x <- try(t.test(rain.data$rain[rain.data$month == j]), silent = T)
  y <- mean(rain.data$rain[rain.data$month == j], na.rm = T)
  z <- sd
  count <- count + 1
  # check if try works
  if(class(x) != "try-error"){
    # store upper and lower
    ci.months.lower[count] <- try(x$conf.int[1], silent = T)
    ci.months.upper[count] <- try(x$conf.int[2], silent = T)
  }

  ci.months.lower[count] <- 
  ci.months.upper[count] <-  
  # store means unified over years
  month.means.total[count] <- y
}

# plot conf ints
plotCI(month.means.total, uiw = ci.months.upper, liw = ci.months.lower,
       ylab = "Rainfall", xlab = "Months",
       main = "Confidence Intervals for Daily Mean Rainfall by Month")

# print out table of mean daily rain by month by year
cat("\nMean daily rainfall by month:\n")
kMonths <- c("Yr  ", "Jan", "Feb", "Mar", "Apr", "May",
             "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
cat(sprintf("%s  ", kMonths), fill=80)

for(i in 1:num.years){
    cat(sprintf("%3i", years.vec[i]))
  for(j in 1:12){
    cat(sprintf("%6.2f", month.means[(j+(i-1)*12)]))
  }
    cat("\n")
}


total <- "Tot. "
cat("\n")
cat(sprintf("%3s", total))
cat(sprintf("%5.2f", month.means.total))
cat("\n")
