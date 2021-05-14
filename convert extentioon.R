# PROJECT CRS WITH R

# set working directory
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# load library
library(raster)
library(rgdal)
library(maptools)


# path to aggregated layers
path2agglayers<-"./layers/lidar/aggregated_vars/"
# clipping shapefile
path2clipshp<-"./layers/lidar/clipping_area.shp"

# Load environmental variables
layers<-list.files(path=path2agglayers,pattern='asc', full.names=TRUE)
# Join all rasters in a single object
egv<-stack(layers)
# check the structure of the rasters
print(egv)

# Load study area shapefile
shp<-readOGR(path2clipshp)

# CRS: COORDINATE REFERENCE SYSTEM
# project vectorial data
# get projection data
projection(egv)
projection(shp)

# compare projections
identicalCRS(egv,shp)

# check CRS
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# assign CRS to a raster file
crs(egv) <- "+init=epsg:23029"
crs(egv) <- "+init=epsg:23029 +proj=utm +zone=29 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs"
projection(egv)

# assign CRS to vector file
crs(shp) <-"+init=epsg:23029"
projection(shp)

# compare projections
identicalCRS(egv,shp)

# project raster to another CRS
egv_wgs<-projectRaster(egv,crs="+init=epsg:4326")
egv_wgs

# project vector to another CRS
shp_wgs<-spTransform(shp,crs("+init=epsg:4326"))
projection(shp_wgs)
shp_wgs

# plot raster and shapefile
plot(egv_wgs)
plot(shp_wgs)

# compare projections
x11()
par(mfrow=c(1,2))
plot(egv[[1]])
plot(egv_wgs[[1]])
# Reset par function
par(mfrow=c(1,1))


# get the extension from raster
e<-extent(egv)
e
# transform the extension to coordinates
ec<-bbox(e)
ec
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')
p
x11()
plot(p)