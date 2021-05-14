#####################################################
################ BIOMOD2 SCRIPT #####################
#####################################################

# Set your paths
#setwd("/home/neftali/Documents/lectures/enm/intro")
setwd("C:/Users/nefta/Documents/lectures/enm/intro")

# path to species
path2sp<-"./layers/worldclim/species/biomod_rana.csv"
# path to layers
path2layers<-"./layers/worldclim/current"
# path to working directory
path2work<-"./biomod/"
# path to Biomod models
path2models<-"./biomod/Rana/models/allmodels"
# path to current models
path2current<-"./biomod/Rana/proj_current"
# path to future 2020 laters
path2020<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2020"
# path to future 2020 models
path2models2020<-"./biomod/Rana/proj_future2020"
# path to future 2050 laters
path2050<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2050"
# path to future 2050 models
path2models2050<-"./biomod/Rana/proj_future2050"
# path to future 2080 laters
path2080<-"./layers/worldclim/future/Iberia_wcB2a_10_hadcm3_2080"
# path to future 2080 models
path2models2080<-"./biomod/Rana/proj_future2080"
# path to pseudo-absence models
path2psmodels<-"./biomod/Rana/models/all_ps_models"
# path to species without absences
path2sp2<-"./layers/worldclim/species/biomod_rana2.csv"

###################################################
###  1: loading_data

# load the library
library(biomod2)

# load our species data
DataSpecies <- read.csv(path2sp,sep=',')
# Check species data
head(DataSpecies)
#plot species data
qplot(x=X_WGS84,y=Y_WGS84,colour=Rana,data=DataSpecies)

# the name of studied species
myRespName <- 'Rana'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])
myResp

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]
# check coordinates
str(myRespXY)
head(myRespXY)

# Environmental variables
layers<-stack(list.files(path=path2layers,pattern='asc',full.names=TRUE ))
# Check layers structure
print(layers)
# Plot layers
plot(layers)
# Descriptive statistics for environmental layers
summary(layers)
# Select variables to include in the model
names(layers)
myExpl<-layers[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check layers structure
print(myExpl)
# Plot environmental variables on geographical space
plot(myExpl)

###################################################
###  2: formating_data

# Check help to know how to define pseudo-absences
?BIOMOD_FormatingData
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName
#                                     PA.nb.rep = 1, # number of pseudo-absence datasets
#                                     PA:nb.absences = 200, # number of pseudo-absence records
#                                     PA.strategy = 'random' # pseudo-absences are selected randomly
                                    )

# print_formating_data
myBiomodData

#: plot_formating_data
plot(myBiomodData)


###################################################
###  3: modeling_options

# Defining Models Options using default options
# Consult help
?BIOMOD_ModelingOptions
myBiomodOption <- BIOMOD_ModelingOptions()

# Setting working directory
setwd(path2work)

###################################################
###  4: modeling

# Computing the models 
# See help
?BIOMOD_Modeling
myBiomodModelOut <- BIOMOD_Modeling( 
                      myBiomodData, # data including species records, species name, and variables
                      models = c('GLM','GBM','GAM','CTA','ANN','SRE','FDA','MARS','RF'), # modelling methods
                      models.options = myBiomodOption, # options for modelling
                      NbRunEval=1, # number of Evaluation run
                      DataSplit=70, # % of data used as training
                      VarImport=3, # number of permutation to estimate var importance
                      models.eval.meth = c('TSS','ROC','KAPPA'), # evaluation metrics
                      SaveObj = TRUE, # keep all results and outputs on hard drive
                      rescal.all.models = TRUE, # all models scaled with a binomial GLM: the range of the scale is 0-1000
                      do.full.models = TRUE, # models calibrated and evaluated with the whole dataset
                      modeling.id = "allmodels") # ID (=name) of modeling procedure

# files created on hard drive
list.files(path2models)

# modeling_summary
myBiomodModelOut 


###################################################
###  5: modeling evaluations

# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
myBiomodModelEval

# print the dimnames of this object
dimnames(myBiomodModelEval)

# print the TSS scores of Random Forest
myBiomodModelEval["TSS","Testing.data","RF",,]

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


###################################################
###  6: projection of current models

# projection over the study area under current conditions
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut, # modelling results
  new.env = myExpl, # environmental variables (here, current climate variables)
  proj.name = 'current', # name of the projections
  selected.models = 'all', # models to be projected
  binary.meth = 'TSS', # a vector of a subset of models evaluation method computed before
  build.clamping.mask = T, # if TRUE, a clamping mask will be saved on hard drive different
  output.format = '.img') # the format of the GIS files: RData, .grd or .img

# summary of created object
myBiomodProj

# files created on hard drive
list.files(path2current)


###################################################
###  7: ensemble modeling

?BIOMOD_EnsembleModeling
myBiomodEM <- BIOMOD_EnsembleModeling( 
                     modeling.output = myBiomodModelOut, # model results
                     chosen.models = 'all', # models to be included when ensembling
                     em.by='all', # flag defining the way the models will be combined to build the ensemble models: 'PA_dataset+repet' (default), 'PA_dataset+algo', 'PA_dataset', 'algo', 'all'
                     # See the vignette: http://127.0.0.1:9144/help/library/biomod2/doc/EnsembleModelingAssembly.pdf
                     eval.metric = c('TSS'), # evaluation metric used to build ensemble models
                     eval.metric.quality.threshold = c(0.7), # If not NULL, the minimum scores below which models will be excluded of the ensemble-models building
                     prob.mean = T, # estimate the mean probabilities across predictions
                     prob.cv = T, # estimate the coefficient of variation across predictions
                     prob.ci = T, # estimate the confidence interval around the prob.mean
                     prob.ci.alpha = 0.05, # significance level for estimating the confidence interval. Default = 0.05
                     prob.median = T, # estimate the mediane of probabilities
                     committee.averaging = T, # estimate the committee averaging across predictions
                     # The committee averaging score is then the average of binary predictions
                     prob.mean.weight = T, # estimate the weighted sum of probabilities
                     prob.mean.weight.decay = 'proportional' ) # define the relative importance of the weights

# print summary                     
myBiomodEM
                     
# get evaluation scores
get_evaluations(myBiomodEM)

###################################################
###  9: EnsembleForecasting_current

myBiomodEF <- BIOMOD_EnsembleForecasting( 
  EM.output = myBiomodEM,
  projection.output = myBiomodProj,
  output.format = '.img')

# print summary
myBiomodEF

# files created on hard drive
list.files(path2current)


###################################################
###  10: projection to future conditions

# 2020
# load environmental variables for the future
layers.2020<-stack(list.files(path=path2020,pattern='asc',full.names=TRUE))
# Check layers structure
print(layers.2020)
# Descriptive statistics for environmental layers
summary(layers.2020)
# Select variables to include in the model
names(layers.2020)
myExpl2020<-layers.2020[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check layers structure
print(myExpl2020)
# Plot environmental variables on geographical space
plot(myExpl2020)

myBiomodProj2020 <- BIOMOD_Projection(
                              modeling.output = myBiomodModelOut,
                              new.env = myExpl2020,
                              proj.name = 'future2020',
                              selected.models = 'all',
                              binary.meth = 'TSS',
                              build.clamping.mask = T,
                              output.format = '.img')

# print summary
myBiomodProj2020

# files created on hard drive
list.files(path2models2020)

#2050
# load environmental variables for the future. 
layers.2050<-stack(list.files(path=path2050,pattern='asc',full.names=TRUE))
# Check layers structure
print(layers.2050)
# Descriptive statistics for environmental layers
summary(layers.2050)
# Select variables to include in the model
names(layers.2050)
myExpl2050<-layers.2050[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check layers structure
print(myExpl2050)
# Plot environmental variables on geographical space
plot(myExpl2050)

myBiomodProj2050 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl2050,
  proj.name = 'future2050',
  selected.models = 'all',
  binary.meth = 'TSS',
  build.clamping.mask = T,
  output.format = '.img')

# print summary
myBiomodProj2050

# files created on hard drive
list.files(path2models2050)

#2080
# load environmental variables for the future. 
layers.2080<-stack(list.files(path=path2080,pattern='asc',full.names=TRUE))
# Check layers structure
print(layers.2080)
# Descriptive statistics for environmental layers
summary(layers.2080)
# Select variables to include in the model
names(layers.2080)
myExpl2080<-layers.2080[[c("bio_03","bio_07","bio_08","bio_10","bio_16","bio_18")]]
# Check layers structure
print(myExpl2080)
# Plot environmental variables on geographical space
plot(myExpl2080)

myBiomodProj2080 <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl2080,
  proj.name = 'future2080',
  selected.models = 'all',
  binary.meth = 'TSS',
  build.clamping.mask = T,
  output.format = '.img')

# print summary
myBiomodProj2080

# files created on hard drive
list.files(path2models2080)

# Plot current and future models
plot(myBiomodProj)
plot(myBiomodProj2020)
plot(myBiomodProj2050)
plot(myBiomodProj2080)

# Plot current and future models
plot(myBiomodProj,str.grep='GLM')
plot(myBiomodProj2020,str.grep='RUN1_GLM')
plot(myBiomodProj2050,str.grep='GLM')
plot(myBiomodProj2080,str.grep='GLM')


# Reset par function
par(mfrow=c(1,1))


###################################################

#### Biomod2 with pseudo-absences

###################################################
###  1: loading_data

# load the library
library(biomod2)

# load our species data
DataSpecies <- read.csv(path2sp2,sep=',')
# Check species data
head(DataSpecies)
#plot species data
qplot(x=X_WGS84,y=Y_WGS84,colour=Rana,data=DataSpecies)

# the name of studied species
myRespName <- 'Rana'

# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])
myResp

# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]
# check coordinates
str(myRespXY)
head(myRespXY)

###  2: formating_data

# Check help to know how to define pseudo-absences
?BIOMOD_FormatingData

myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                     expl.var = myExpl,
                                     resp.xy = myRespXY,
                                     resp.name = myRespName,
                                     PA.nb.rep = 1, # number of pseudo-absence datasets
                                     PA.nb.absences = 800, # number of pseudo-absence records
                                     PA.strategy = 'disk', # pseudo-absences are selected randomly
                                     PA.dist.min = 0.08333334, # minimal distance to presences 
                                     PA.dist.max = NULL # maximal distance to presences 
                                     )

# print_formating_data
myBiomodData

#: plot_formating_data
plot(myBiomodData)


###################################################
###  3: modeling_options

# Defining Models Options using default options
# Consult help
?BIOMOD_ModelingOptions
myBiomodOption <- BIOMOD_ModelingOptions()

# Setting working directory
setwd(path2work)

###################################################
###  4: modeling

# Computing the models 
# See help
?BIOMOD_Modeling
myBiomodModelOut <- BIOMOD_Modeling( 
  myBiomodData, # data including species records, species name, and variables
  models = c('GLM'), # modelling methods
  models.options = myBiomodOption, # options for modelling
  NbRunEval=3, # number of Evaluation run
  DataSplit=70, # % of data used as training
  VarImport=3, # number of permutation to estimate var importance
  models.eval.meth = c('TSS','ROC'), # evaluation metrics
  SaveObj = TRUE, # keep all results and outputs on hard drive
  rescal.all.models = TRUE, # all models scaled with a binomial GLM: the range of the scale is 0-1000
  do.full.models = TRUE, # models calibrated and evaluated with the whole dataset
  modeling.id = "all_ps2_models") # ID (=name) of modeling procedure

# files created on hard drive
list.files(path2psmodels)

# modeling_summary
myBiomodModelOut 


###################################################
###  5: modeling evaluations

# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
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


###################################################
###  6: projection of current models

# projection over the study area under current conditions
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut, # modelling results
  new.env = myExpl, # environmental variables (here, current climate variables)
  proj.name = 'current', # name of the projections
  selected.models = 'all', # models to be projected
  binary.meth = 'ROC', # a vector of a subset of models evaluation method computed before
  build.clamping.mask = T, # if TRUE, a clamping mask will be saved on hard drive different
  output.format = '.img') # the format of the GIS files: .RData, .grd, or .img

# summary of created object
myBiomodProj

# files created on hard drive
list.files(path2current)
