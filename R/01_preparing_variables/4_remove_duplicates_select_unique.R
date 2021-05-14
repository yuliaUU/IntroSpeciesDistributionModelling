# Load library to read species shapefiles and rasters
library(raster)
library(maptools)
library(rgdal)
# Load library for remove duplicates
library(ecospat)

# Read the shapefile of points
shp <- readShapePoints("/home/neftali/Downloads/C_viridis/C.shp")
# Plot the shapefile
plot(shp)
# Transform the shapefile in a dataframe
spp<-as.data.frame(shp)
# Check the dataframe
# Read the first 5 lines of the dataframe
head(spp)
# Select only the species and coordinate fields
spp<-spp[,c(2,5,6)]
# Read the first 5 lines of the dataframe
head(spp)
# Rename fields
colnames(spp)<-c("species","x","y")
# Read the first 5 lines of the dataframe
head(spp)

# Load dplyr library to delete duplicated records
library(dplyr)
# Check the structure of spp
str(spp)
# Select unique records
spp<-distinct(spp)
# Check the structure of spp
str(spp)
# Count the number of rows of the dataframe
row1<-nrow(spp)
row1
# Remove species occurrences in a dataframe that are closer to each other than a specified distance threshold
# dfvar: dataframe object
# min.dist define the distance threshold
# colxy: columns with coordinates
# colvar: column with the name of the species
# plot: map the selected and excluded points
occ.sp<-ecospat.occ.desaggregation(spp,min.dist=1)
# Check the dataframe
# Read the first 5 lines of the dataframe
head(occ.sp)
# Count the number of rows of the dataframe with the selected points
row2<-nrow(occ.sp)
row2
# Get coordinates from dataframe
occ.x<-occ.sp[,1]
occ.y<-occ.sp[,2]
# Plot the selected points
plot(occ.x,occ.y)
#export the dataframe with the selected points
# Get name of the band
write.csv(occ.sp,file=paste("XXXXX.csv",sep=''),row.names=FALSE)