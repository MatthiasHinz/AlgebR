library(sp)
library(gstat)
# if needed, install mss:
#
# install.packages("devtools")
# devtools::install_github("edzer/mss")
library(mss)

#----------------------------------------------------
if(TRUE){
  rm(list=ls()) #make sure to start from a clean workspace
}
source("graphFunctions.R")

# Initialize provenance tracking

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
captureSemantics(init_model) <-TRUE

modelSemivariogram = function(pointData) {
  n = names(pointData@observations)
  if (length(n) > 1)
    warning("taking first attribute variable")
  f = as.formula(paste(n[1], "~1")) # which variable to model? take first.
  init = init_model(pointData)
  fit.variogram(variogram(f, pointData@observations), init)
}
captureSemantics(modelSemivariogram) <-TRUE

getInterpolator = function(params, pointData) {
  if (!is(params, "variogramModel"))
    warning("getInterpolator: params should be of class variogramModel")
  #if (!is(pointData, "SField"))
  #	warning("getInterpolator: pointData should be of class SField")
  out=function(locOfInterest) {
    n = names(pointData@observations)[1] # which variable to model? take first.
    f = as.formula(paste(n, "~ 1")) 
    interpolate(f, pointData, locOfInterest, model = params)
    # interpolate(f, pointData, locOfInterest, model = params, ndiscr=4, verbose=TRUE)
  }
  captureSemantics(out) <-TRUE
  attr(out, "semantics") <- "SField"
  # is, strictly not S -> Q but S -> (S,Q)
  return(out)
}
captureSemantics(getInterpolator) <-TRUE

captureSemantics(geometry) <- TRUE

SFieldData <- SField
captureSemantics(SFieldData) <- TRUE

algebr$enableProvenance()


# Run analysis
#-----------------------------------------------

# load meuse data from package sp in current session:
demo(meuse, ask=FALSE, echo=FALSE)
meuse$lzinc = log(meuse$zinc)

zincPointData = SFieldData(meuse["lzinc"], meuse.area)
#class(zincPointData) # of class SField
#plot(zincPointData)


interpolator = getInterpolator(modelSemivariogram(zincPointData), zincPointData)
#class(interpolator) # untyped, but is S -> Q

locInterest = SFieldData(geometry(meuse.grid), geometry(meuse.grid), cellsArePoints = TRUE)
intZincPointData = interpolator(locInterest, semantics = "S -> Q")
#class(intZincPointData)
spplot(intZincPointData@observations[1])


## Create / visualize graph
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="interpolation.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="interpolation.svg", fileType="svg")
system(command = "dot -Tpdf interpolation.dot -o interpolation.pdf")
setwd("../")

#for exploration and analytics, if the graph has an error...
#algebr$compareVE(algebr$scriptGraph)

