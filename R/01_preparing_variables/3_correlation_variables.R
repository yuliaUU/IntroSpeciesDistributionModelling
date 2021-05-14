########################## CORRELATION #############################

# Load libraries
library(raster)
library(maptools)
library(rgdal)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to clipped layers
path2cliplayers<-"./layers/lidar/clipped_vars/"
# path to correlation folder
path2corr<-"./layers/lidar/"

# Load environmental variables
layers<-list.files(path=path2cliplayers,pattern='asc',full.names=TRUE)
# Join all rasters in a single object
egv<-stack(layers)
# check the structure of the rasters
print(egv)
# check descriptive statistics of the rasters
summary(egv)
# Plot the stack
plot(egv)

# Calculate Pearson's correlation among variables
# Pearson's correlation is parametric
pearson<-layerStats(egv,'pearson',na.rm=T)
# Check correlation results
print(pearson)

# Transform correlation results in a dataframe
cor.df<-as.data.frame(pearson)
# Check dataframe structure
head(cor.df)
# Export dataframe
write.table(cor.df,paste(path2corr,'var_correlations.csv',sep=''),sep=",")

# Transfrom correlation matrix to distances
var.dist <- abs(as.dist(cor.df))
# Calculate dendrogram based on distance (less distance = more correlation)
var.cluster <- hclust(1-var.dist)
# Plot dendrogram
plot(var.cluster)
abline(h=0.25, lty=2, lwd=2) # variables that have a correlation < 0.75
# Export correlation graph
jpeg(paste(path2corr,'correlation_clusters.jpg',sep=''))
plot(var.cluster)
abline(h=0.25, lty=2, lwd=2) # variables that have a correlation < 0.75
dev.off()
