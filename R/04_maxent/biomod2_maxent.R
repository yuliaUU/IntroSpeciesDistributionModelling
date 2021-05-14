#####################################################
############### MAXENT WITH BIOMOD2 #################
#####################################################

# load the library
library(biomod2)

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to species
path2sp<-"./layers/worldclim/species/biomod_rana.csv"
# path to layers
path2layers<-"./layers/worldclim/current"
# path to maxent jar file
path2maxent<-'./R/04_maxent/maxent/'
# path to working directory
path2work<-'./results/biomod/maxent/'
# path to future 2020 laters
path2020<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2020"
# path to future 2020 models
path2models2020<-"./results/biomod/maxent/Rana/proj_future2020"
# path to future 2050 laters
path2050<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2050"
# path to future 2050 models
path2models2050<-"./results/biomod/maxent/Rana/proj_future2050"
# path to future 2080 laters
path2080<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2080"
# path to future 2080 models
path2models2080<-"./results/biomod/maxent/Rana/proj_future2080"

########################################
# load our species data
DataSpecies <- read.csv(path2sp,sep=',')
# Check species data
head(DataSpecies)
str(DataSpecies)

# the name of studied species
myRespName <- 'Rana'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]


# load the environmental raster layers
layers<-stack(list.files(path=path2layers,pattern='asc',full.names=TRUE ))
# Descriptive statistics for environmental layers
summary(layers)
# Select variables to include in the model
names(layers)
myExpl<-layers[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Plot environmental variables on geographical space
plot(myExpl)

# Formating data
myBiomodData <- BIOMOD_FormatingData(
  resp.var = myResp, 
  expl.var = myExpl, 
  resp.xy = myRespXY, 
  resp.name = myRespName)

# print_formating_data
myBiomodData

#: plot_formating_data
plot(myBiomodData)

# Defining MAXENT Mododelling options 
# Consult help
?BIOMOD_ModelingOptions
myBiomodOption <- BIOMOD_ModelingOptions(MAXENT.Phillips = list(path_to_maxent.jar=path2maxent, 
                         memory_allocated=1024,
                         maximumiterations = 500, 
                         visible = TRUE, 
                         linear = TRUE,
                         quadratic = TRUE, 
                         product = TRUE, 
                         threshold = TRUE, 
                         hinge = TRUE, 
                         lq2lqptthreshold = 80, 
                         l2lqthreshold = 10, 
                         hingethreshold = 15, 
                         beta_threshold = -1, 
                         beta_categorical = -1, 
                         beta_lqp = -1, 
                         beta_hinge = -1, 
                         betamultiplier = 1,
                         defaultprevalence = 0.5))

# Setting working directory
setwd(path2work)

# Computing the models 
# See help
?BIOMOD_Modeling
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, 
                                     models =  'MAXENT.Phillips',
                                     models.options = myBiomodOption, 
                                     NbRunEval=1, # number of Evaluation run
                                     DataSplit=70, # % of data used as training
                                     VarImport=3, # number of permutation to estimate var importance
                                     models.eval.meth = 'ROC', # evaluation metrics
                                     SaveObj = TRUE,# keep all results and outputs on hard drive
                                     rescal.all.models = FALSE, # all models scaled with a binomial GLM
                                     modeling.id = "maxent") # ID (=name) of modeling procedure

# modeling_summary
myBiomodModelOut

# get all models evaluation 
myBiomodModelEval<-get_evaluations(myBiomodModelOut) 
myBiomodModelEval

# print the dimnames of this object
dimnames(myBiomodModelEval)

# print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)

# save models evaluation scores and variables importance on hard drive
capture.output(get_evaluations(myBiomodModelOut),
               file=file.path(myRespName, 
                              paste(myRespName,"_formal_models_evaluation.txt", sep="")))

capture.output(get_variables_importance(myBiomodModelOut),
               file=file.path(myRespName, 
                              paste(myRespName,"_formal_models_variables_importance.txt", sep="")))  

# Project models over studied area 
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut, # modelling results
  new.env = myExpl, # environmental variables (here, current climate variables)
  proj.name = 'current', # name of the projections
  selected.models = 'all', # models to be projected
  binary.meth = 'ROC', # a vector of a subset of models evaluation method computed before
  build.clamping.mask = T, # if TRUE, a clamping mask will be saved on hard drive different
  output.format = '.img') # the format of the GIS files: .RData, .grd or .img

# summary of created object
myBiomodProj

###################################################
# Projection to future conditions

# Load environmental variables for 2020
layers2020<-stack(list.files(path=path2020,pattern='asc',full.names=TRUE))
# Descriptive statistics for environmental layers
summary(layers2020)
# Select variables to include in the model
names(layers2020)
egv2020<-layers2020[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]

# Plot environmental variables on geographical space
plot(egv2020)

myBiomodProj2020 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = egv2020,
  proj.name = 'future2020',
  selected.models = 'all',
  binary.meth = 'ROC',
  build.clamping.mask = T,
  output.format = '.img')

# print summary
myBiomodProj2020

# files created on hard drive
list.files(path2models2020)

# Load environmental variables for 2050
layers2050<-stack(list.files(path=path2050,pattern='asc',full.names=TRUE))
# Descriptive statistics for environmental layers
summary(layers2050)
# Select variables to include in the model
names(layers2050)
egv2050<-layers2050[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Plot environmental variables on geographical space
plot(egv2050)

myBiomodProj2050 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = egv2050,
  proj.name = 'future2050',
  selected.models = 'all',
  binary.meth = 'ROC',
  build.clamping.mask = T,
  output.format = '.img')

# print summary
myBiomodProj2050

# files created on hard drive
list.files(path2models2050)

# Load environmental variables for 2080
layers2080<-stack(list.files(path=path2080,pattern='asc',full.names=TRUE))
# Descriptive statistics for environmental layers
summary(layers2080)
# Select variables to include in the model
names(layers2080)
egv2080<-layers2080[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Plot environmental variables on geographical space
plot(egv2080)

myBiomodProj2080 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = egv2080,
  proj.name = 'future2080',
  selected.models = 'all',
  binary.meth = 'ROC',
  build.clamping.mask = T,
  output.format = '.img')

# print summary
myBiomodProj2080

# files created on hard drive
list.files(path2models2080)

# Plot Bioclim current and future models
x11()
plot(myBiomodProj)
plot(myBiomodProj2020)
plot(myBiomodProj2050)
plot(myBiomodProj2080)

