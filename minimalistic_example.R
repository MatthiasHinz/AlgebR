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
setwd("output")
toFile(gRlayout , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
system(command = "dot -Tpng myDerivationGraph.dot -o myDerivationGraph.png")
setwd("..")
algebr$versions(meuse)
