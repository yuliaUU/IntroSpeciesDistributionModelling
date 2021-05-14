########################## REMOVE DUPLICATES #############################


# Load library to read species shapefiles and rasters
library(raster)
library(maptools)
library(rgdal)
# Load library for remove duplicates
library(ecospat)

# Set your paths

#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to species folder
path2sp<-"./layers/lidar/species"
# path to species csv folder
path2spcsv<-"./layers/lidar/species/csv/"

# path to species shapefiles
list.shp<-list.files(path2sp,pattern='shp',full.names=TRUE)
list.shp

# Create a empty dataframe to store the results
df=NULL

for (i in list.shp) { 
  # Read the shapefile of points
  shp <- readOGR(i)
  # Plot the shapefile
  X11()
  plot(shp)
  # Transform the shapefile in a dataframe
  spp<-as.data.frame(shp)
  # Check the dataframe
  # Read the first 5 lines of the dataframe
  head(spp)
  # Count the number of rows of the dataframe
  row1<-nrow(shp)
  row1
  # Remove species occurrences in a dataframe that are closer to each other than a specified distance threshold
  # dfvar: dataframe object
  # min.dist define the distance threshold
  # colxy: columns with coordinates
  # colvar: column with the name of the species
  # plot: map the selected and excluded points
  occ.sp<-ecospat.occ.desaggregation(spp,min.dist=10)
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
  X11()
  plot(occ.x,occ.y)
  #export the dataframe with the selected points
  # Get name of the band
  name<-substr(i, 66, 90)
  write.csv(occ.sp,file=paste(path2spcsv,name,".csv",sep=''),row.names=FALSE)
  # Add the number of selected points to the null dataframe
  df<-rbind(df,data.frame(levels(occ.sp$sp),row1,row2))
}

#export the null dataframe
write.csv(df,file=paste(path2spcsv,"df_shp.csv",sep=''),row.names=FALSE)

