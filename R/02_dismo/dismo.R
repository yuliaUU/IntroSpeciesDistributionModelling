######################################################
################## DISMO SCRIPT ######################
######################################################

# Load dismo package
library(raster)
library(dismo)
library(rgdal)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to original layers
path2layers<-"./layers/worldclim/current"
# path to sp
path2sp<-"./layers/worldclim/species/dismo_rana.txt"
# path to training data
path2train<-"./results/dismo/rana_data.train.txt"
# path to test data
path2test<-"./results/dismo/rana_data.test.txt"
# path to background data
path2backg<-"./results/dismo/rana_data.backgr.txt"
# path to bioclim model
path2bioclim<-"./results/dismo/bioclim/bc_rana"
# path to bioclim thresholded model
path2bioclimtr<-"./results/dismo/bioclim/tr_bc_rana"
# path to domain model
path2domain<-"./results/dismo/domain/dm_rana"
# path to dismo thresholded model
path2domaintr<-"./results/dismo/domain/tr_bc_rana"
# path to future 2020 layers
path2020<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2020"
# path to future 2050 layers
path2050<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2050"
# path to future 2080 layers
path2080<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2080"
# path to projected 2020 bioclim model
path2bioclim2020<-"./results/dismo/bioclim/bc_rana_2020"
# path to projected 2050 bioclim model
path2bioclim2050<-"./results/dismo/bioclim/bc_rana_2050"
# path to projected 2080 bioclim model
path2bioclim2080<-"./results/dismo/bioclim/bc_rana_2080"
# path to projected 2020 domain model
path2domain2020<-"./results/dismo/domain/dm_rana_2020"
# path to projected 2050 domain model
path2domain2050<-"./results/dismo/domain/dm_rana_2050"
# path to projected 2080 domain model
path2domain2080<-"./results/dismo/domain/dm_rana_2080"
  
######################################################
# PREPARING DATA

# Load environmental variables
layers<-list.files(path=path2layers,pattern='asc',full.names=TRUE)
# Make the stack 
egv<-stack(layers)
# Check egv structure
print(egv)
# Descriptive statistics for environmental layers
summary(egv)
# Select variables to include in the model
names(egv)
egv<-egv[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check egv structure
print(egv)
# Plot environmental variables on geographical space
X11()
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

# Plot training, test, and background data on geographical space
par(mfrow=c(1,3))
plot(data.train,main="training data for Rana")
plot(data.test,main="test data for Rana")
plot(backgr,main="background data for Rana")

# Reset par function
par(mfrow=c(1,1))

######################################################
# BIOCLIM

# Calculate model
bc<-bioclim(egv,data.train)
bc
# Plot presence and absence (background) data
# Colinearity in the environmental data
X11()
pairs(bc)
# Plot the model in the environmental space
# a: variable in X axis
# b: variable in Y axis
# p: percentile threshold
plot(bc,a=1,b=2,p=1)
plot(bc,a=1,b=2,p=0.95)
plot(bc,a=1,b=2,p=0.90)
plot(bc,a=1,b=2,p=0.85)
plot(bc,a=1,b=2,p=0.80)
plot(bc,a=1,b=2,p=0.75)
plot(bc,a=1,b=2,p=0.70)
plot(bc,a=1,b=2,p=0.65)
plot(bc,a=1,b=2,p=0.60)
plot(bc,a=1,b=2,p=0.50)
# Project the model
p.bc<-predict(bc,egv)
p.bc
# Plot the model in the geographical space
plot(p.bc)
# Plot variable response curves
X11()
response(bc)
# Export Bioclim model
# check raster formats available to export the results
?writeFormats
writeRaster(p.bc,path2bioclim,format="HFA",overwrite=TRUE)
# Evaluate the model
e.bc<-dismo::evaluate(data.test,backgr,bc,egv)
e.bc
# Check the evaluation results
str(e.bc)
# Boxplot of presence and absence suitable values
# blue are absences, red are presences
X11()
boxplot(e.bc,col=c('blue','red'))
# Density plot of presence and absence suitable values
# blue are absences, red are presences
X11()
density(e.bc)
# Evaluation plots
# ROC
plot(e.bc,'ROC')
# CCR: Correct classification rate
plot(e.bc,'CCR')
# TPR: True positive rate
plot(e.bc,'TPR')
# TNR: True negative rate
plot(e.bc,'TNR')
# FPR: False positive rate
plot(e.bc,'FPR')
# FNR: False negative rate
plot(e.bc,'FNR')
# PPP: Positive predictive power
plot(e.bc,'PPP')
# NPP: Negative predictive power
plot(e.bc,'NPP')
# kappa: Cohen’s kappa
plot(e.bc,'kappa')

# Obtain the threshold
# kappa: the threshold at which kappa is highest ("max kappa")
# spec_sens: the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest
# no_omission: the highest threshold at which there is no omission
# prevalence: modeled prevalence is closest to observed prevalence
# equal_sens_spec: equal sensitivity and specificity
# sensitivity: fixed (specified) sensitivity
tr.b<-threshold(e.bc,'spec_sens')
# Get the value of the threshold
tr.b
# Calculate the thresholded model
tr.bc<-p.bc>tr.b
tr.bc
# Plot the p/a model in the geographical space
plot(tr.bc)
# Export p/a model
writeRaster(tr.bc,path2bioclimtr,format="HFA",overwrite=TRUE)

# Plot raw and p/a models
x11(width = 10,height = 6)
par(mfrow=c(1,2))
plot(p.bc,main='Bioclim, raw values')
points(data.train,pch='+')
plot(tr.bc,main='presence/absence')
points(data.train,pch='+')
# Reset par function
par(mfrow=c(1,1))

######################################################
# DOMAIN

# Calculate the model
dm<-domain(egv,data.train)
dm
# Plot presence and absence (background) data
# Colinearity in the environmental data
X11()
pairs(dm)
# Project the model
p.dm<-predict(egv,dm)
p.dm
# Plot the model in the geographical space
plot(p.dm)
# Plot variable response curves
X11()
response(dm)

# Export Domain model
writeRaster(p.dm,path2domain,format="HFA",overwrite=TRUE)
# Evaluate the model
e.dm<-dismo::evaluate(data.test,backgr,dm,egv)
e.dm
# Check the evaluation results
str(e.dm)
# Boxplot of presence and absence suitable values
# blue are absences, red are presences
X11()
boxplot(e.dm,col=c('blue','red'))
# Density plot of presence and absence suitable values
# blue are absences, red are presences
X11()
density(e.dm)
# Evaluation plots
# ROC
X11()
plot(e.dm,'ROC')
# CCR: Correct classification rate
plot(e.dm,'CCR')
# TPR: True positive rate
plot(e.dm,'TPR')
# TNR: True negative rate
plot(e.dm,'TNR')
# FPR: False positive rate
plot(e.dm,'FPR')
# FNR: False negative rate
plot(e.dm,'FNR')
# PPP: Positive predictive power
plot(e.dm,'PPP')
# NPP: Negative predictive power
plot(e.dm,'NPP')
# kappa: Cohen’s kappa
plot(e.dm,'kappa')

# Obtain the threshold
# kappa: the threshold at which kappa is highest ("max kappa")
# spec_sens: the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest
# no_omission: the highest threshold at which there is no omission
# prevalence: modeled prevalence is closest to observed prevalence
# equal_sens_spec: equal sensitivity and specificity
# sensitivty: fixed (specified) sensitivity
tr.d<-threshold(e.dm,'spec_sens')
# Get the value of the threshold
tr.d
# Calculate the thresholded model
tr.dm<-p.dm>tr.d
tr.dm
# Plot the p/a model in the geographical space
X11()
plot(tr.dm)
# Export p/a model
writeRaster(tr.dm,path2domaintr,format="HFA",overwrite=TRUE)

x11(width = 10,height = 6)
# Plot raw and p/a models
par(mfrow=c(1,2))
plot(p.dm,main='Domain, raw values')
points(data.train,pch='+')
plot(tr.dm,main='presence/absence')
points(data.train,pch='+')
# Reset par function
par(mfrow=c(1,1))

######################################################

# Plot Bioclim and Domain
X11()
par(mfrow=c(1,2))
plot(tr.bc,main='Bioclim')
plot(tr.dm,main='Domain')
# Reset par function
par(mfrow=c(1,1))


######################################################
# ENSEMBLE MODELS

# Mean raw model
avg<-((p.bc+p.dm)/2)
x11()
plot(avg)

# Mean p/a model
avg2<-((tr.dm+tr.bc)/2)
plot(avg2)

# Mean model weighted by AUC values
avg3<-(((p.bc*e.bc@auc)+(p.dm*e.dm@auc))/2)
plot(avg3)

# Plot mean models
par(mfrow=c(1,2))
plot(avg)
plot(avg2)

# Difference between p/a models
par(mfrow=c(1,1))
diff.m<-abs(tr.dm-tr.bc)
plot(diff.m)

######################################################
# PROJECTING MODELS TO OTHER SCENARIOS

# Load environmental variables for 2020
layers2020<-list.files(path=path2020,pattern='asc',full.names=TRUE)
# Make the stack
egv2020<-stack(layers2020)
# Check egv structure
print(egv2020)
# Descriptive statistics for environmental layers
summary(egv2020)
# Select variables to include in the model
names(egv2020)
egv2020<-egv2020[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check egv structure
print(egv2020)
# Plot environmental variables on geographical space
plot(egv2020)

# Load environmental variables for 2050
layers2050<-list.files(path=path2050,pattern='asc',full.names=TRUE)
# Make the stack
egv2050<-stack(layers2050)
# Check egv structure
print(egv2050)
# Descriptive statistics for environmental layers
summary(egv2050)
# Select variables to include in the model
names(egv2050)
egv2050<-egv2050[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check egv structure
print(egv2050)
# Plot environmental variables on geographical space
plot(egv2050)

# Load environmental variables for 2080
layers2080<-list.files(path=path2080,pattern='asc',full.names=TRUE)
# Make the stack
egv2080<-stack(layers2080)
# Check egv structure
print(egv2080)
# Descriptive statistics for environmental layers
summary(egv2080)
# Select variables to include in the model
names(egv2080)
egv2080<-egv2080[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check egv structure
print(egv2080)
# Plot environmental variables on geographical space
plot(egv2080)

#########################################
# Project models to future scenarios

# BIOCLIM
# Project the model to 2020
p.bc.20 = predict(egv2020,bc)
p.bc.20
# Plot the model in the geographical space
plot(p.bc.20)
# Export Bioclim 2020 model
writeRaster(p.bc.20,path2bioclim2020,format="HFA",overwrite=TRUE)

# Project the model to 2050
p.bc.50 = predict(egv2050,bc)
p.bc.50
# Plot the model in the geographical space
plot(p.bc.50)
# Export Bioclim 2050 model
writeRaster(p.bc.50,path2bioclim2050,format="HFA",overwrite=TRUE)

# Project the model to 2080
p.bc.80 = predict(egv2080,bc)
p.bc.80
# Plot the model in the geographical space
plot(p.bc.80)
# Export Bioclim 2080 model
writeRaster(p.bc.80,path2bioclim2080,format="HFA",overwrite=TRUE)

# Plot Bioclim current and future models
par(mfrow=c(2,2))
plot(p.bc,main='Bioclim current')
plot(p.bc.20,main='Bioclim 2020')
plot(p.bc.50,main='Bioclim 2050')
plot(p.bc.80,main='Bioclim 2080')
# Reset par function
par(mfrow=c(1,1))

# DOMAIN
# Project the model to 2020
p.dm.20 = predict(egv2020,dm)
p.dm.20
# Plot the model in the geographical space
plot(p.dm.20)
# Export domain 2020 model
writeRaster(p.dm.20,path2domain2020,format="HFA",overwrite=TRUE)

# Project the model to 2050
p.dm.50 = predict(egv2050,dm)
p.dm.50
# Plot the model in the geographical space
plot(p.dm.50)
# Export domain 2050 model
writeRaster(p.dm.50,path2domain2050,format="HFA",overwrite=TRUE)

# Project the model to 2080
p.dm.80 = predict(egv2080,dm)
p.dm.80
# Plot the model in the geographical space
plot(p.dm.80)
# Export domain 2080 model
writeRaster(p.dm.80,path2domain2080,format="HFA",overwrite=TRUE)

# Plot domain current and future models
par(mfrow=c(2,2))
plot(p.dm,main='Domain current')
plot(p.dm.20,main='Domain 2020')
plot(p.dm.50,main='Domain 2050')
plot(p.dm.80,main='Domain 2080')
# Reset par function
par(mfrow=c(1,1))

# Plot bioclim and domain current and future models
par(mfrow=c(2,4))
plot(p.bc,main='Bioclim current')
plot(p.bc.20,main='Bioclim 2020')
plot(p.bc.50,main='Bioclim 2050')
plot(p.bc.80,main='Bioclim 2080')
plot(p.dm,main='Domain current')
plot(p.dm.20,main='Domain 2020')
plot(p.dm.50,main='Domain 2050')
plot(p.dm.80,main='Domain 2080')
# Reset par function
par(mfrow=c(1,1))

# Threshold all models
tr.bc.20<-p.bc.20>tr.b
tr.bc.50<-p.bc.50>tr.b
tr.bc.80<-p.bc.80>tr.b
tr.dm.20<-p.dm.20>tr.d
tr.dm.50<-p.dm.50>tr.d
tr.dm.80<-p.dm.80>tr.d

# Plot thresholded bioclim and domain current and future models
par(mfrow=c(2,4))
plot(tr.bc,main='Bioclim current')
plot(tr.bc.20,main='Bioclim 2020')
plot(tr.bc.50,main='Bioclim 2050')
plot(tr.bc.80,main='Bioclim 2080')
points(data.train,pch='+')
plot(tr.dm,main='Domain current')
plot(tr.dm.20,main='Domain 2020')
plot(tr.dm.50,main='Domain 2050')
plot(tr.dm.80,main='Domain 2080')
points(data.train,pch='+')
# Reset par function
par(mfrow=c(1,1))