######################## AGGREGATING RASTERS #####################

# Load libraries
library(raster)
library(rgdal)
library(maptools)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to original layers
path2layers<-"./layers/lidar/original_vars"
# path to aggregated layers
path2agglayers<-"./layers/lidar/aggregated_vars/"

# Load environmental variables
layers<-list.files(path=path2layers,pattern='asc',full.names=TRUE)
# Join all rasters in a single object
egv<-stack(layers)
# check the structure of the rasters
print(egv)
# check descriptive statistics of the rasters
summary(egv)
# Plot the stack
plot(egv)

# Aggregate raster
# object; number of pixels to aggregate; aggregation function
agg<-aggregate(egv,10,fun=mean)
# check the structure of the aggregated rasters
print(agg)
# check descriptive statistics of the aggregated rasters
summary(agg)

# Loop to export sequentially aggregated stack
for (i in 1:6) { # there are 6 bands
  # Select a band sequentially
  r<-agg[[i]]
  # Plot the selected band
  plot(r,main=r@data@names)
  # Get name of the band
  name<-r@data@names
  # export the raster to a folder using paste
  writeRaster(r, filename=paste(path2agglayers,name,sep=''), format="ascii", overwrite=TRUE)
}
