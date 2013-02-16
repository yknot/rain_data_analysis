# code to analyze rain data from bocas del toro field station
#
# Andrew Yale
#
# used for EWB-RPI in their Panama Project


# read in data
# data in format of year,month,day,rain
rain.data <- read.csv("~/R/rain_data_analysis/rain_days_split2.csv",
                      header = T)


# simple boxplot of rain seperated into months
###### boxplot(rain.data$rain ~ rain.data$month)

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

month.means

