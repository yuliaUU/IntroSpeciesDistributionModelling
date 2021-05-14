#####################################################
############### MAXENT WITH DISMO ###################
#####################################################

# Load libraries
library(raster)
library(rgdal)
library(maptools)

# Load dismo package
library(dismo)
library(rJava)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to species
path2sp<-"./layers/worldclim/species/dismo_rana.txt"
# path to layers
path2layers<-"./layers/worldclim/current"
# path to data train database
path2train<-"./results/dismo/rana_data.train.txt"
# path to data test database
path2test<-"./results/dismo/rana_data.test.txt"
# path to data background database
path2backg<-"./results/dismo/rana_data.backgr.txt"
# path to maxent jar file in dismo
path2jar<-"./results/dismo/maxent"
# path to maxent tif model
path2tif<-'./results/dismo/maxent/maxent_prediction_rana.tif'
# path to maxent model
path2maxent<-"./results/dismo/maxent/me.rana"
# path to maxent thresholded model
path2maxenttr<-"./results/dismo/maxent/tr_me_rana"
# path to future 2020 laters
path2020<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2020"
# path to future 2020 models
path2models2020<-"./results/dismo/maxent/me_rana_2020"
# path to future 2050 laters
path2050<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2050"
# path to future 2050 models
path2models2050<-"./results/dismo/maxent/me_rana_2050"
# path to future 2080 laters
path2080<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2080"
# path to future 2080 models
path2models2080<-"./results/dismo/maxent/me_rana_2080"

######################################################
# PREPARING DATA

# Load environmental variables
layers<-list.files(path=path2layers,pattern='asc',full.names=TRUE)
# Make the stack
egv<-stack(layers)
# Check the stack structure
print(egv)
# Descriptive statistics for environmental layers
summary(egv)
# Select variables to include in the model
names(egv)
egv<-egv[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check the stack structure
print(egv)
# Plot environmental variables on geographical space
plot(egv)

# Load species records
data.sp<-read.csv(path2sp,sep=",",header=T)
# Check the structure of the species dataframe
head(data.sp)
# Plot species records on geographical space
plot(data.sp)

# Split species records in training and testing data
## Using sample function
## 0.3 corresponds to testing proportion
indexes = sample(1:nrow(data.sp),size=0.3*nrow(data.sp))
## Split data in training and testing
data.train = data.sp[-indexes,]
data.test = data.sp[indexes,]
# Check training dataframe
head(data.train)
str(data.train)
# Check testing dataframe
head(data.test)
str(data.test)
# Extract background data
# Create a cloud of random points
backgr<-as.data.frame(randomPoints(egv,500))
# Check background dataframe
head(backgr)
str(backgr)
# Export training, test, and background data
write.table(data.train,file=path2train,sep='')
write.table(data.test,file=path2test,sep='')
write.table(backgr,file=path2backg,sep='')

######################################################
# MAXENT WITH DISMO

# only run if the maxent.jar file is available,in the right folder
# Copy maxent.jar to /home/neftali/R/x86_64-pc-linux-gnu-library/3.2/dismo/java
jar<-paste(system.file(package="dismo"),"/java/maxent.jar",sep='')

# Calculate model with MAXENT parameters
# Choose parameters
# See maxent function in R help
# See maxent parameters in maxent help
?dismo::maxent
me<-maxent(egv,data.train,args=c("randomseed","writeclampgrid","betamultiplier=1","responsecurves","jackknife","pictures","replicates=1"),removeDuplicates=TRUE,path=path2jar)
# Calculate model
# 'biome' is a categorical variable
#me<-maxent(egv,data.train,factors='biome')
# See the maxent results in a browser
me
# Plot importance of each variable
plot(me)
# Response curves
response(me)
# Project the model
p.me<-predict(me,egv,args=c("outputformat=logistic"),progress='text',filename=path2tif,overwrite=TRUE)
plot(p.me)
points(data.train)
# Export Maxent model
writeRaster(p.me,path2maxent,format="HFA",overwrite=TRUE)
# Evaluate the model
e.me<-dismo::evaluate(me,p=data.test,a=backgr,x=egv)
e.me
# Check the evaluation results
str(e.me)
# Boxplot of presence and absence suitable values
# blue are absences, red are presences
boxplot(e.me,col=c('blue','red'))
# Density plot of presence and absence suitable values
# blue are absences, red are presences
density(e.me)
# Evaluation plots
# ROC
plot(e.me,'ROC')
# CCR: Correct classification rate
plot(e.me,'CCR')
# TPR: True positive rate
plot(e.me,'TPR')
# TNR: True negative rate
plot(e.me,'TNR')
# FPR: False positive rate
plot(e.me,'FPR')
# FNR: False negative rate
plot(e.me,'FNR')
# PPP: Positive predictive power
plot(e.me,'PPP')
# NPP: Negative predictive power
plot(e.me,'NPP')
# kappa: Cohen's kappa
plot(e.me,'kappa')

# Obtain the threshold
# kappa: the threshold at which kappa is highest ("max kappa")
# spec_sens: the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest
# no_omission: the highest threshold at which there is no omission
# prevalence: modeled prevalence is closest to observed prevalence
# equal_sens_spec: equal sensitivity and specificity
# sensitivity: fixed (specified) sensitivity
tr<-threshold(e.me,'prevalence')
# Get the value of the threshold
tr
# Get the value of all thresholds
tr<-threshold(e.me)
tr[,6]
# Calculate the thresholded model
tr.me<-p.me>(tr[,6])
# Plot the p/a model in the geographical space
tr.me
x11()
plot(tr.me)
# Export p/a model
writeRaster(tr.me,path2maxenttr,format="HFA",overwrite=TRUE)

# Plot raw and p/a models
par(mfrow=c(1,2))
plot(p.me,main='Maxent, raw values')
points(data.train,pch='+')
plot(tr.me,main='presence/absence')
points(data.test,pch='+')

# Reset par function
par(mfrow=c(1,1))


######################################################
# PROJECTING MODELS TO OTHER SCENARIOS

# Load environmental variables for 2020
layers2020<-list.files(path=path2020,pattern='asc',full.names=TRUE)
# Make the stack
egv2020<-stack(layers2020)
# Check the stack structure
print(egv2020)
# Descriptive statistics for environmental layers
summary(egv2020)
# Select variables to include in the model
egv2020<-egv2020[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check the stack structure
print(egv2020)
# Plot environmental variables on geographical space
plot(egv2020)

# Load environmental variables for 2050
layers2050<-list.files(path=path2050,pattern='asc',full.names=TRUE)
# Make the stack
egv2050<-stack(layers2050)
# Check the stack structure
print(egv2050)
# Descriptive statistics for environmental layers
summary(egv2050)
# Select variables to include in the model
egv2050<-egv2050[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check the stack structure
print(egv2050)

# Load environmental variables for 2080
layers2080<-list.files(path=path2080,pattern='asc',full.names=TRUE)
# Make the stack
egv2080<-stack(layers2080)
# Check the stack structure
print(egv2080)
# Descriptive statistics for environmental layers
summary(egv2080)
# Select variables to include in the model
egv2080<-egv2080[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check the stack structure
print(egv2080)
# Plot environmental variables on geographical space
plot(egv2080)

# Project the model to 2020
p.me.20 = predict(egv2020,me)
p.me.20
# Plot the model in the geographical space
x11()
plot(p.me.20)
# Export Bioclim model
writeRaster(p.me.20,path2models2020,format="HFA",overwrite=TRUE)

# Project the model to 2050
p.me.50 = predict(egv2050,me)
p.me.50
# Plot the model in the geographical space
x11()
plot(p.me.50)
# Export Bioclim model
writeRaster(p.me.50,path2models2050,format="HFA",overwrite=TRUE)

# Project the model to 2080
p.me.80 = predict(egv2080,me)
p.me.80
# Plot the model in the geographical space
plot(p.me.80)
# Export Bioclim model
writeRaster(p.me.80,path2models2080,format="HFA",overwrite=TRUE)

# Plot Bioclim current and future models
par(mfrow=c(2,2))
plot(p.me,main='Bioclim current')
plot(p.me.20,main='Bioclim 2020')
plot(p.me.50,main='Bioclim 2050')
plot(p.me.80,main='Bioclim 2080')
# Reset par function
par(mfrow=c(1,1))

