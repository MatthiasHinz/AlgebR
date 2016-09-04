if(FALSE){
  rm(list=ls())
}

source("graphFunctions.R")

captureSemantics(rnorm, procedureName="rname") <- TRUE
sum = function(x){base::sum(x)}
captureSemantics(sum, semantics = "any set -> any", procedureName="rname") <- TRUE
algebr$enableProvenance()
t=1
attr(t, which="semantics") <- "test"
t=t+2
t=round(abs(rnorm(t)))
t=rnorm(sum(t), semantics="any -> any set")
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="miniexample.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="miniexample.svg", fileType="svg")
system(command = "dot -Tpdf miniexample.dot -o miniexpample.pdf")
setwd("../")
algebr$versions(t)
getSemanticPedigree(t)
