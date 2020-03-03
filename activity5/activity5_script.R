library(lubridate)

#####         QUESTION 2      #####




#read in streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)
#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay - 1)/366),
                       datD$year + ((datD$decDay - 1/365)))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay - 1) / 366),
                       datP$year + ((datP$decDay - 1) / 365)) 


#####         QUESTION 3/4      #####



#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

length(datD$stage)
length(datP$HPCP)

help(expression)
help(paste)



#####         QUESTION 5/6      #####
datD$month <- month(datesD)


#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$month), FUN="mean")
colnames(aveF) <- c("month","monthlyAve")
sdF <- aggregate(datD$discharge, by=list(datD$month), FUN="sd")
colnames(sdF) <- c("month","monthlySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$month,aveF$monthlyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(-20,50),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$month, rev(aveF$month)),#x coordinates
        c(aveF$monthlyAve-sdF$monthlySD,rev(aveF$monthlyAve+sdF$monthlySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,12, by=1), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(-20,50, by=10),
     seq(-20,50, by=10),
     las = 2)#show ticks at 90 degree angle

#Add 2017 line
aveF2017 <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$month[datD$year == 2017]), FUN="mean")
colnames(aveF2017) <- c("month","monthlyAve")
lines(aveF2017$month,aveF2017$monthlyAve,col="red")

legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA,1),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")#no legend border



#####         QUESTION 7     #####
datP$dates <- round_date(ymd_hm(datP$DATE),unit="day")
datP$datesFactor <- as.factor(datP$dates)
datD$dates <- datesD

fullData <- function(date){
        dischargeDate = subset(datP,dates==date)
        return (nrow(dischargeDate) >= 24)
}

array <- c()
for(i in levels(datP$datesFactor)){
        if(fullData(ymd(i)) == TRUE){
                array <- c(array, i)
        }
}

plot(datD$doy,datD$discharge)
for (i in array) {
        points(datD$doy[datD$dates == i],datD$discharge[datD$dates == i],col="red")
}

#####         QUESTION 8     #####

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#subsest discharge and precipitation within range of interest
hydroD2 <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2010,]
hydroP2 <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2010,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
        polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
                  hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
                c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl2),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#####         QUESTION 9     #####

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)

#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

datD2016 <- subset(datD,year == "2016")

season2016 <- function(doy){
        if(doy < 80 | doy >= 356){
                result <- "Winter"
        }
        else if (doy >= 80 & doy < 172){
                result <- "Spring"
        }
        else if (doy >= 172 & doy < 286){
                result <- "Summer"
        }
        else{
                result <- "Fall"
        }
        return(result)
}

datD2016$seasonPlot <- as.factor(unlist(lapply(datD2016$doy,season2016)))

ggplot(data = datD2016,aes(seasonPlot,discharge)) + 
        geom_violin() + ggtitle("Violin Plot for 2016") +
        xlab("Season of the Year") + ylab("Discharge for Season")

datD2017 <- subset(datD,year == "2017")

season2017 <- function(doy){
        if(doy < 79 | doy >= 355){
                result <- "Winter"
        }
        else if (doy >= 79 & doy < 171){
                result <- "Spring"
        }
        else if (doy >= 171 & doy < 285){
                result <- "Summer"
        }
        else{
                result <- "Fall"
        }
        return(result)
}

datD2017$seasonPlot <- as.factor(unlist(lapply(datD2017$doy,season2017)))

ggplot(data = datD2017,aes(seasonPlot,discharge)) + 
        geom_violin() + ggtitle("Violin Plot for 2017") +
        xlab("Season of the Year") + ylab("Discharge for Season")
