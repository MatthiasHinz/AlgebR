#rm(list=ls())

source("graphFunctions.R")
# install.packages(c("sp", "gstat"))
library(sp)
library(gstat)
# if needed, install mss:
#
# install.packages("devtools")
# devtools::install_github("edzer/mss")
library(mss)



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

captureSemantics(init_model) <-TRUE
captureSemantics(modelSemivariogram) <-TRUE
captureSemantics(getInterpolator) <-TRUE
captureSemantics(geometry) <- TRUE
captureSemantics(SField) <- TRUE
captureSemantics(GridTopology) <- TRUE
captureSemantics(SpatialGrid) <- TRUE
captureSemantics(aggregate) <- TRUE
captureSemantics(SLattice) <- TRUE
#-------------------------------------------------------------------------------------
algebr$enableProvenance()
#-------------------------------------------------------------------------------------

# load meuse data from package sp in current session:
demo(meuse, ask=FALSE, echo=FALSE)
meuse$lzinc = log(meuse$zinc)

zincPointData = SField(meuse["lzinc"], meuse.area)
class(zincPointData) # of class SField
plot(zincPointData)


interpolator = getInterpolator(modelSemivariogram(zincPointData), zincPointData)
class(interpolator) # untyped, but is S -> Q

locInterest = SField(geometry(meuse.grid), geometry(meuse.grid), cellsArePoints = TRUE)
intZincPointData = interpolator(locInterest)
class(intZincPointData)
spplot(intZincPointData@observations[1])

# for blocks:
blockInterest = SLattice(geometry(meuse.grid))
blockIntZincPointData = interpolator(blockInterest)
class(blockIntZincPointData) # should be SLattice

# blocks to point:
#  first, generate some block data, by aggregating point data to blocks
off = gridparameters(meuse.grid)$cellcentre.offset + 20
gt = GridTopology(off, c(1000,1000), c(4,6))
SG = as(SpatialGrid(gt, CRS(proj4string(meuse))), "SpatialPolygons")
zincBlockData = aggregate(zincPointData, SLattice(SG))
zincBlockData = zincBlockData[!is.na(zincBlockData[[1]]),]

interpolator = getInterpolator(modelSemivariogram(zincPointData), zincBlockData)
pointFromBlock = interpolator(zincPointData)
class(pointFromBlock)
spplot(pointFromBlock@observations[1])

# blocks to block:

interpolator = getInterpolator(modelSemivariogram(zincPointData), zincBlockData)
blockFromBlock = interpolator(blockInterest[1:10,])
class(pointFromBlock)
spplot(blockFromBlock@observations[1])

#-----------------------------------------------------------------------------------------
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="blockkriging.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="blockkriging.svg", fileType="svg")
system(command = "dot -Tpdf blockkriging.dot -o blockkriging.pdf")
setwd("../")
