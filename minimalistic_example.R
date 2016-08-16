#rm(list=ls())
source("https://github.com/MatthiasHinz/AlgebR/raw/master/graphFunctions.R")
library(sp)
log = function(x){
  return(base::log(x))
}
captureSemantics(log) <- TRUE

algebr$enableProvenance()
demo(meuse, ask=FALSE, echo=FALSE)
meuse$lzinc = log(meuse$zinc)
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
toFile(gRlayout , layoutType="dot", filename="output/myDerivationGraph.svg", fileType="svg")
algebr$versions(meuse)
