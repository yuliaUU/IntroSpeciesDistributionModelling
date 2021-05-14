######################################################
# DENSITY PLOTS BACKGROUND/PRESENCE DATA

# Load libraries
library(raster)
library(maptools)
library(rgdal)

# Load dismo package
library(dismo)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to clipped layers
path2cliplayers<-"./layers/lidar/clipped_vars/"
# path to species csv file
path2spcsv<-"./layers/lidar/species/csv/Podarcis bocagei.shp.csv"
# path to species dataframe
path2spdb<-"./layers/lidar/species/data_pb.txt"

# Load environmental variables
layers<-list.files(path=path2cliplayers,pattern='asc',full.names=TRUE)
# make the stack
egv<-stack(layers)
# check rasters
print(egv)
# Descriptive statistics for environmental layers
summary(egv)
# Plot environmental variables on geographical space
X11()
plot(egv)

# Load species records
data.sp<-read.csv(path2spcsv,sep=",",header=T)
# Check the structure of the species dataframe
head(data.sp)
# Select coordinate fields on species records
data.sp<-data.sp[c(2,3)]
# Check the structure of the species dataframe
head(data.sp)
# Plot species records on geographical space
X11()
plot(data.sp)

# Extract background data
# Create a cloud of random points
backgr<-as.data.frame(randomPoints(egv,500))
# Check background dataframe
head(backgr)
str(backgr)

# Extract environmental data on species records
data.pres<-extract(egv,data.sp)
# Check presence dataframe
str(data.pres)
# Join coordinates to presence records
data.pres<-cbind(data.pres,data.sp)
# Delete NA values from data frame
data.pres<-na.omit(data.pres)
# Check presence dataframe
str(data.pres)

# Extract environmental data on background records
data.backgr<-extract(egv,backgr)
# Delete NA values from data frame
data.backgr<-na.omit(data.backgr)
# Check background dataframe
str(data.backgr)
# Join coordinates to background records
data.backgr<-cbind(data.backgr,backgr)
# Check background dataframe
str(data.backgr)

# It is necessary to change the names of fields x/y to long/lat
# If not, coloum names will not match
# Change number of coloums depeding on the number of egv
colnames(data.backgr)[7]<-"long"
colnames(data.backgr)[8]<-"lat"
# Check background dataframe
str(data.backgr)
# It is necessary to change the names of fields x/y to long/lat
# If not, coloum names will not match
# Change number of coloums depeding on the number of egv
colnames(data.pres)[7]<-"long"
colnames(data.pres)[8]<-"lat"
# Check background dataframe
str(data.pres)

# Join presence and background dataframes
data.pb<-data.frame(rbind(data.pres,data.backgr))
# Check presence+background dataframe
str(data.pb)
# Export presence and background dataframe
write.table(data.pb,file=path2spdb,sep=',')

# Represent species records against background
# Presences are blue
for (j in names(egv)) {
  X11()
  hist(data.backgr[,j],freq=T,main=j) # returns the density data
  hist(data.pres[,j],freq=T,col='red',add=T) # returns the density data
}

