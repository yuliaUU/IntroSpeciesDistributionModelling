########################## CLIPPING RASTERS #############################

# Load libraries
library(raster)
library(maptools)
library(rgdal)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to aggregated layers
path2agglayers<-"./layers/lidar/aggregated_vars/"
# clipping shapefile
path2clipshp<-"./layers/lidar/clipping_area.shp"
# path to clipped layers
path2cliplayers<-"./layers/lidar/clipped_vars/"

# Load environmental variables
layers<-list.files(path=path2agglayers,pattern='asc',full.names=TRUE)
# Join all rasters in a single object
egv<-stack(layers)
# check the structure of the rasters
print(egv)
# check descriptive statistics of the rasters
summary(egv)
# Plot the stack
#x11() #if the following plot is not represented correctly: with x11 a new window is open
plot(egv)

# Load study area shapefile
shp<-readOGR(path2clipshp)
# Plot the shapefile
plot(shp)

# Loop to clip each raster sequentially
for (i in layers) { # look for each ascii file in layers
  #load raster
  egv.i<-raster(i)
  # clip raster
  # the raster to be clipped; the clipping feature; snap determines in which direction the extent should be aligned
  # to the nearest border, inwards or outwards
  clip.r<-crop(egv.i, shp, snap="in")
  # plot the clipped raster
  X11()
  plot(clip.r,main=clip.r@data@names)
  #mask raster with the shapefile to create NoData
  mask.r<-mask(clip.r, shp)
  # plot the masked raster
  X11()
  plot(mask.r,main=mask.r@data@names)
  # Get name of the band
  name<-mask.r@data@names
  # export the raster to a folder using paste
  writeRaster(mask.r,paste(path2cliplayers,"clip_",name,sep=''),format="ascii", overwrite=TRUE)
}

# plot all clipped variables
files<-list.files(path=path2cliplayers,pattern='asc',full.names=TRUE)
# Join all rasters in a single object
clips<-stack(files)
# Plot the stack
plot(clips)
