
library(SPODT)
library(tree)

rm(list=ls())
source("graphFunctions.R")

captureSemantics(spTransform) <- FALSE
captureSemantics(`coordinates<-`) <- FALSE
captureSemantics(`proj4string<-`) <- FALSE
captureSemantics(CRS) <- FALSE

captureSemantics(test.spodt) <- TRUE
captureSemantics(spodtSpatialLines) <- TRUE
captureSemantics(spodt) <- TRUE

captureSemantics(tree) <- FALSE
captureSemantics(partition.tree) <- FALSE
algebr$enableProvenance()

####################################################################
########            Section 4: Data examples                    ####
########  4.1 Clustering malaria episodes, (Bandiagara, Mali)   ####
####################################################################

data("dataMALARIA")

#projection
coordinates(dataMALARIA) <- c("x", "y")
proj4string(dataMALARIA) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
dataMALARIA <- spTransform(dataMALARIA, CRS("+proj=merc +datum=WGS84 +ellps=WGS84"))

########################################################################################
### Spatial oblique decision tree: using SpODT algorithme for spatial classification ###

spodt.results <- spodt(z ~ 1, data = dataMALARIA, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)

#partition obtained
spodt.results@partition

#R2 for the partition
spodt.results@R2

#classification tree
spodt.tree(spodt.results)

#creation of spatial lines between classes and plot
SSL.result <- spodtSpatialLines(spodt.results, dataMALARIA)
plot(SSL.result)
#adding each location
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))


#Spatial oblique decision tree with no graft option
spodt.results0 <- spodt(z ~ 1, data = dataMALARIA, graft = 0, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)
SSL.result0 <- spodtSpatialLines(spodt.results0, dataMALARIA)
plot(SSL.result0)
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))

#Test for the SpODT classification
test.spodt(z ~ 1, data = dataMALARIA, spodt.results@R2, "rpois", c(length(dataMALARIA@data$loc), mean(dataMALARIA@data$z)), 99, weight = TRUE, graft = 0.13, level.max = 7, min.parent = 25, min.child = 2, rtwo.min = 0.01)


########################################################
###  Using CART algorithm for spatial classification ###

cart.results <- tree(z ~ x + y, data=dataMALARIA)

# classification tree
plot(cart.results)
text(cart.results)

# classification map
partition.tree(cart.results)
points(dataMALARIA, cex = log(dataMALARIA@data$z*10))

####################################################################
########    4.2 Different cluster shapes and levels     ############
####################################################################

###########################################################################################
# Ball shape situation:
# Clustered data with a low level inside a centered ball shape, and a high level outside.

#
#### beta= 0 => no cluster
#

data("dataBALL0")
coordinates(dataBALL0) <- c("x", "y")

#SPODT classification
spodt.ball0 <- spodt(z ~ 1, data = dataBALL0, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.ball0, dataBALL0)
plot(SSL.result)
points(dataBALL0, cex = log(dataBALL0@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataBALL0, spodt.ball0@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.ball0 <- tree(z ~ x + y, data=dataBALL0)

# CART - classification map
partition.tree(cart.ball0)
points(dataBALL0, cex = log(dataBALL0@data$z*20))

#
#### beta= 0.5 => medium risk outside
#

data("dataBALL0_5")
coordinates(dataBALL0_5) <- c("x", "y")

#SPODT classification
spodt.ball0_5 <- spodt(z ~ 1, data = dataBALL0_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.ball0_5, dataBALL0_5)
plot(SSL.result)
points(dataBALL0_5, cex = log(dataBALL0_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataBALL0_5, spodt.ball0_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.ball0_5 <- tree(z ~ x + y, data=dataBALL0_5)

# CART - classification map
partition.tree(cart.ball0_5)
points(dataBALL0_5, cex = log(dataBALL0_5@data$z*20))

#
#### beta= 1.5 => high risk outside
#

data("dataBALL1_5")
coordinates(dataBALL1_5) <- c("x", "y")

#SPODT classification
spodt.ball1_5 <- spodt(z ~ 1, data = dataBALL1_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.ball1_5, dataBALL1_5)
plot(SSL.result)
points(dataBALL1_5, cex = log(dataBALL1_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataBALL1_5, spodt.ball1_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.ball1_5 <- tree(z ~ x + y, data=dataBALL1_5)

# CART - classification map
partition.tree(cart.ball1_5)
points(dataBALL1_5, cex = log(dataBALL1_5@data$z*20))

#
#### beta= 2 => very high risk outside
#

data("dataBALL2")
coordinates(dataBALL2) <- c("x", "y")

#SPODT classification
spodt.ball2 <- spodt(z ~ 1, data = dataBALL2, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.ball2, dataBALL2)
plot(SSL.result)
points(dataBALL2, cex = log(dataBALL2@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataBALL2, spodt.ball2@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.ball2 <- tree(z ~ x + y, data=dataBALL2)

# CART - classification map
partition.tree(cart.ball2)
points(dataBALL2, cex = log(dataBALL2@data$z*20))

###########################################################################################
# Rotated Square shape situation:
# Clustered data with a high level inside a rotated square shape, and a low level outside.

#
#### beta= 0 => no cluster
#

data("dataSQUARE0")
coordinates(dataSQUARE0) <- c("x", "y")

#SPODT classification
spodt.square0 <- spodt(z ~ 1, data = dataSQUARE0, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.square0, dataSQUARE0)
plot(SSL.result)
points(dataSQUARE0, cex = log(dataSQUARE0@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataSQUARE0, spodt.square0@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.square0 <- tree(z ~ x + y, data=dataSQUARE0)

# CART - classification map
partition.tree(cart.square0)
points(dataSQUARE0, cex = log(dataSQUARE0@data$z*20))

#
#### beta= 0.5 => medium risk inside
#

data("dataSQUARE0_5")
coordinates(dataSQUARE0_5) <- c("x", "y")

#SPODT classification
spodt.square0_5 <- spodt(z ~ 1, data = dataSQUARE0_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.square0_5, dataSQUARE0_5)
plot(SSL.result)
points(dataSQUARE0_5, cex = log(dataSQUARE0_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataSQUARE0_5, spodt.square0_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.square0_5 <- tree(z ~ x + y, data=dataSQUARE0_5)

# CART - classification map
partition.tree(cart.square0_5)
points(dataSQUARE0_5, cex = log(dataSQUARE0_5@data$z*20))

#
#### beta= 1.5 => high risk inside
#

data("dataSQUARE1_5")
coordinates(dataSQUARE1_5) <- c("x", "y")

#SPODT classification
spodt.square1_5 <- spodt(z ~ 1, data = dataSQUARE1_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.square1_5, dataSQUARE1_5)
plot(SSL.result)
points(dataSQUARE1_5, cex = log(dataSQUARE1_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataSQUARE1_5, spodt.square1_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.square1_5 <- tree(z ~ x + y, data=dataSQUARE1_5)

# CART - classification map
partition.tree(cart.square1_5)
points(dataSQUARE1_5, cex = log(dataSQUARE1_5@data$z*20))

#
#### beta= 2 => very high risk inside
#

data("dataSQUARE2")
coordinates(dataSQUARE2) <- c("x", "y")

#SPODT classification
spodt.square2 <- spodt(z ~ 1, data = dataSQUARE2, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.square2, dataSQUARE2)
plot(SSL.result)
points(dataSQUARE2, cex = log(dataSQUARE2@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataSQUARE2, spodt.square2@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.square2 <- tree(z ~ x + y, data=dataSQUARE2)

# CART - classification map
partition.tree(cart.square2)
points(dataSQUARE2, cex = log(dataSQUARE2@data$z*20))

###########################################################################################
# "V" shape situation:
# Clustered data with a high level under a "V" shape border, and a low level above

#
#### beta= 0 => no cluster
#

data("dataV0")
coordinates(dataV0) <- c("x", "y")

#SPODT classification
spodt.V0 <- spodt(z ~ 1, data = dataV0, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.V0, dataV0)
plot(SSL.result)
points(dataV0, cex = log(dataV0@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataV0, spodt.V0@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.V0 <- tree(z ~ x + y, data=dataV0)

# CART - classification map
partition.tree(cart.V0)
points(dataV0, cex = log(dataV0@data$z*20))

#
#### beta= 0.5 => medium risk under the border
#

data("dataV0_5")
coordinates(dataV0_5) <- c("x", "y")

#SPODT classification
spodt.V0_5 <- spodt(z ~ 1, data = dataV0_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.V0_5, dataV0_5)
plot(SSL.result)
points(dataV0_5, cex = log(dataV0_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataV0_5, spodt.V0_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.V0_5 <- tree(z ~ x + y, data=dataV0_5)

# CART - classification map
partition.tree(cart.V0_5)
points(dataV0_5, cex = log(dataV0_5@data$z*20))

#
#### beta= 1.5 => high risk under the border
#

data("dataV1_5")
coordinates(dataV1_5) <- c("x", "y")

#SPODT classification
spodt.V1_5 <- spodt(z ~ 1, data = dataV1_5, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.V1_5, dataV1_5)
plot(SSL.result)
points(dataV1_5, cex = log(dataV1_5@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataV1_5, spodt.V1_5@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.V1_5 <- tree(z ~ x + y, data=dataV1_5)

# CART - classification map
partition.tree(cart.V1_5)
points(dataV1_5, cex = log(dataV1_5@data$z*20))

#
#### beta= 2 => very high risk under the border
#

data("dataV2")
coordinates(dataV2) <- c("x", "y")

#SPODT - classification
spodt.V2 <- spodt(z ~ 1, data = dataV2, graft = 0.2)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.V2, dataV2)
plot(SSL.result)
points(dataV2, cex = log(dataV2@data$z*20))

#SPODT - Test
test.spodt(z ~ 1, data = dataV2, spodt.V2@R2, "runif", c(300,-1,1), 99, graft = 0.2)

#CART
cart.V2 <- tree(z ~ x + y, data=dataV2)

# CART - classification map
partition.tree(cart.V2)
points(dataV2, cex = log(dataV2@data$z*20))

####################################################################
#######    4.3 Spatial partition with a time covariate    ##########
####################################################################

data("dataCOV")
coordinates(dataCOV) <- c("x", "y")

#SPODT - classification
spodt.results.cov <- spodt(z ~ V1, data = dataCOV, weight = TRUE, graft = 0.2, level.max = 5, min.parent = 10, min.child = 5, rtwo.min = 0.001)

#SPODT - Map
SSL.result <- spodtSpatialLines(spodt.results.cov, dataCOV)
plot(SSL.result)
points(dataCOV, cex = log(dataCOV@data$z*20))

#SPODT - tree
spodt.tree(spodt.results.cov)


#CART
cart.cov <- tree(z ~ x + y + V1, data=dataCOV)

# CART - tree
plot(cart.cov)
text(cart.cov)

algebr$disableProvenance()

gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="SPODT-complete.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="SPODT-complete.svg", fileType="svg")
#not useful
#system(command = "dot -Tpdf SPODT-complete.dot -o SPODT-complete.pdf")
setwd("../")
