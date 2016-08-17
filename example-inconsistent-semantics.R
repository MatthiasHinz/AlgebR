if(TRUE){
  source("https://github.com/MatthiasHinz/AlgebR/raw/master/graphFunctions.R")
}else{
  rm(list=ls())
  source("graphFunctions.R")
}

log = function(x){
  return(base::log(x))
}
captureSemantics(log, semantics=c("Q -> Q", "Q set -> Q set")) <- TRUE

algebr$enableProvenance()
t=123
log(t) #no inconsistencies
log(t, c("Q set -> Q set"))
attr(t, "semantics") <-"myType"
log(t)
algebr$disableProvenance()

gRlayout = algebr$getScriptGraph()
setwd("output")
toFile(gRlayout , layoutType="dot", filename="inconsistentGraph.dot", fileType="dot")
system(command = "dot -Tpng inconsistentGraph.dot -o inconsistentGraph.png")
setwd("..")
