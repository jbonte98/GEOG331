#install.packages(c("lubridate"))
library(lubridate)

#create a function. The names of the arguements for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}
#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")
#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])
#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)
#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil moisture
length(which(is.na(datW$soil.moisture)))
#soil temperature
length(which(is.na(datW$soil.temp)))
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)
#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]

#Question 5
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
assert(length(lightscale)==nrow(datW),"This did not have the amount of data needed")
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#Question 6:
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

assert(sum(is.na(datW$air.tempQ2))==sum(is.na(datW$wind.speedQ2)),"Storm values were not excluded from wind speed")

plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Windspeed",
     type="b")

#Question 7

plot(datW$DD, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="b",col="red")
lines(datW$DD, datW$wind.speedQ2, col = "blue")


#Question 8

meanAirTemp <- round(mean(datW$air.temperature, na.rm = TRUE),digits=1)
meanWindSpeed <- round(mean(datW$wind.speed, na.rm = TRUE),digits=2)
meanSoilMoist <- round(mean(datW$soil.moisture, na.rm = TRUE),digits=3)
meanSoilTemp <- round(mean(datW$soil.temp, na.rm = TRUE),digits=1)
experimentTime <- (tail(datW$doy, n = 1) - head(datW$doy, n = 1)) * 24 + (tail(datW$hour, n = 1) - head(datW$hour, n = 1))
numObservations <- length(datW$air.temperature)
totalPrecip <- sum(datW$precipitation, na.rm = TRUE)
summaryExperiment <- data.frame(meanAirTemp=meanAirTemp,meanWindSpeed=meanWindSpeed,meanSoilMoist=meanSoilMoist,
                                meanSoilTemp=meanSoilTemp,experimentTime=experimentTime,totalObservations=numObservations)


#Question 9

par(mfrow=c(2,2))
plot(datW$doy[1:1411], datW$soil.moisture[1:1411], xlab = "Day of Year", ylab = "Soil Moisture",
     type="b",col="red")
plot(datW$doy[1:1411], datW$air.temperature[1:1411], xlab = "Day of Year", ylab = "Air Temperature",
     type="b",col="blue")
plot(datW$doy[1:1411], datW$soil.temp[1:1411], xlab = "Day of Year", ylab = "Soil Temperature",
     type="b",col="green")
plot(datW$doy[1:1411], datW$precipitation[1:1411], xlab = "Day of Year", ylab = "Precipitation",
     type="b",col="orange")
