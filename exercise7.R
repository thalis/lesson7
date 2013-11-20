#Author: Thanos Strantzalis
#Date: November 2013

rm(list = ls())

library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)

data(tura)
tura <- tura/10000

# extract layer names from the tura rasterBrick
sceneID <- names(tura)
# parse these names to extract scene information
sceneinfo <- getSceneinfo(sceneID)

# add a 'year' column to the sceneinfo data.frame
sceneinfo$year <- factor(substr(sceneinfo$date, 1, 4), levels = c(1984:2013))

#prepare the 3 rasterBricks
#year 2000
year2000<-which(sceneinfo$year==2000)
tura2000<-tura[[year2000]]
#year 2005
year2005<-which(sceneinfo$year==2005)
tura2005<-tura[[year2005]]
#year 2010
year2010<-which(sceneinfo$year==2010)
tura2010<-tura[[year2010]]

# median per year
med2000 <- calc(tura2000, fun = median, na.rm = TRUE)
med2005 <- calc(tura2005, fun = median, na.rm = TRUE)
med2010 <- calc(tura2010, fun = median, na.rm = TRUE)

#pack into 1 rasterBrick
med<-brick(med2000,med2005,med2010)

# 1. a plot() or levelplot() of the three mean or median layers from
## 2000, 2005 and 2010
names(med[[1]])<-"MedianNDVI2000"
names(med[[2]])<-"MedianNDVI2005"
names(med[[3]])<-"MedianNDVI2010"
cols <- brewer.pal(11, 'RdYlGn')
rtheme <- rasterTheme(region=cols)
levelplot(x=med,par.settings=rtheme)

# 2.an RGB composite plot of the three layers as follows:
## 2000 (red), 2005 (green) and 2010(blue).
plotRGB(med,1,2,3, stretch="hist")

# possible change between 2000 and 2005
e1<-extent(c(819767.9,820288.8,828786.8,830076.6))
plot(e1,add=TRUE)
# possible change between 2005 and 2010
e2<-extent(c(820784.9,821169.4,828253.5,828613.2))
plot(e2,add=TRUE)

# extract median values for each raster in the selected AOIs
medianAOI1 <- extract(tura, e1, fun = median, na.rm=TRUE)
medianAOI2 <- extract(tura, e2, fun = median, na.rm=TRUE)

# add to new fields for the median values at the sceneinfo
sceneinfo$AOI1<-medianAOI1
sceneinfo$AOI2<-medianAOI2

# 3. time series plots (preferably a facet wrap plot using ggplot())
## for each of the two chosen regions.

# area 1
tsAOI1<-ggplot(data = sceneinfo, aes(x = date, y = AOI1)) +
  geom_point(size = 3,col="red") +
  labs(y = "Median NDVI") +
  scale_y_continuous(limits=c(0.4, 0.9)) +
  theme_bw()
print(tsAOI1)

#area 2
tsAOI1<-ggplot(data = sceneinfo, aes(x = date, y = AOI2)) +
  geom_point(size = 3,col="blue") +
  labs(y = "Median NDVI") +
  scale_y_continuous(limits=c(0.45, 0.9)) +
  theme_bw()
print(tsAOI1)










