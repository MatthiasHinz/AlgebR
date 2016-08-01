library(sp)
library(gstat)
# if needed, install mss:
#
# install.packages("devtools")
# devtools::install_github("edzer/mss")
library(mss)

#----------------------------------------------------

rm(list=ls())
source("graphFunctions.R")

# Initialize provenance tracking
algebr$enableProvenance()
#algebr$disableProvenance()
#algebr$reset()
#algebr$history()
# Load resources

# define helper functions
#---------------------------------------------------
init_model = function(pointData) {
  range = sqrt(sum(apply(bbox(pointData@observations), 1, diff)^2)) / 6
  sill = var(pointData[[1]])
  vgm(2 * sill / 3, "Sph", range, sill / 3) # initial variogram model
}


modelSemivariogram = function(pointData) {
  n = names(pointData@observations)
  if (length(n) > 1)
    warning("taking first attribute variable")
  f = as.formula(paste(n[1], "~1")) # which variable to model? take first.
  init = init_model(pointData)
  fit.variogram(variogram(f, pointData@observations), init)
}

getInterpolator = function(params, pointData) {
  if (!is(params, "variogramModel"))
    warning("getInterpolator: params should be of class variogramModel")
  #if (!is(pointData, "SField"))
  #	warning("getInterpolator: pointData should be of class SField")
  function(locOfInterest) {
    n = names(pointData@observations)[1] # which variable to model? take first.
    f = as.formula(paste(n, "~ 1")) 
    interpolate(f, pointData, locOfInterest, model = params)
    # interpolate(f, pointData, locOfInterest, model = params, ndiscr=4, verbose=TRUE)
  }
  # is, strictly not S -> Q but S -> (S,Q)
}


# Run analysis
#-----------------------------------------------

# load meuse data from package sp in current session:
demo(meuse, ask=FALSE, echo=FALSE)
meuse$lzinc = log(meuse$zinc)

zincPointData = SField(meuse["lzinc"], meuse.area)
#class(zincPointData) # of class SField
#plot(zincPointData)


interpolator = getInterpolator(modelSemivariogram(zincPointData), zincPointData)
#class(interpolator) # untyped, but is S -> Q

locInterest = SField(geometry(meuse.grid), geometry(meuse.grid), cellsArePoints = TRUE)
intZincPointData = interpolator(locInterest)
#class(intZincPointData)
#spplot(intZincPointData@observations[1])


## Create / visualize graph
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="interpolation.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="interpolation.svg", fileType="svg")
setwd("../")
