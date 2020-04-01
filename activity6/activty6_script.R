#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#### Question 1 ####
#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_1966.shp")

g1998 <- readOGR("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_1998.shp")

g2005 <- readOGR("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_2005.shp")

g2015 <- readOGR("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/GNPglaciers/GNPglaciers_2015.shp")

spplot(g1966, "GLACNAME")

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#### Question 2 ####

#read in rgb imagery from landsat
redL <- raster("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/glacier_09_05_14/l08_blue.tif")

rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#### Question 3 ####

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("/Users/jeremybonte/Desktop/Geography/GEOG331/activity6/a06/NDVI/NDVI_",ndviYear[i],".tif"))
}

#Split the graphing area into two graphs
par(mfrow=c(1,2))
#Graph the plot for the 2003 raster data
plot(NDVIraster[[1]])
#Graph the 1966 polygons alongside
plot(g1966, axes = TRUE)

#### Question 4 ####
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#Reformat graphing area to only show one graph
par(mfrow=c(1,1))
#Plot the 2015 raster data
plot(NDVIraster[[14]])
#Superimpose the g2015p data on top of the raster data
plot(g2015p, add = TRUE)


#### Question 5 ####
#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
}   

gAll$percent_change <- (gAll$a1966m.sq - gAll$a2015m.sq)/(gAll$a1966m.sq) * 100
g2015p@data$percent_change <- gAll$percent_change
spplot(g2015p,"percent_change",col="transparent")

#### Question 6 ####
gAllMax <- subset(gAll,percent_change==max(percent_change),select=c(GLACNAME))
g1966Boulder <- subset(g1966, GLACNAME==gAllMax$GLACNAME)
g1998Boulder <- subset(g1998, GLACNAME==gAllMax$GLACNAME)
g2005Boulder <- subset(g2005, GLACNAME==gAllMax$GLACNAME)
g2015Boulder <- subset(g2015, GLACNAME==gAllMax$GLACNAME)

par(mfrow=c(1,1))
plotRGB(rgbL,ext=c(272000,275600,5426000,5428500), stretch="lin",main="Boulder Glacier")
plot(g1966Boulder, col="palegreen2", border=NA, add=TRUE)
plot(g1998Boulder, col="royalblue3", add=TRUE, border=NA)
plot(g2005Boulder, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015Boulder, col="tomato3", add=TRUE, border=NA)

#### Question 7 ####
#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)
#### Question 8 ####
#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#### Question 9 ####
meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)
g2015p@data$meanChange <- meanChange[2:40]
spplot(g2015p,"meanChange",col="transparent")

#### Question 10 ####
#### Question 11 ####
g2015p@data$NDVImean <- 
temp <- ifelse(g2015p@data$NDVImean)
g2015p@data$NDVIcol <- g2015p@data$NDVImean
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
