source("graphFunctions.R")
algebr$enableProvenance()
t=1
attr(t, which="semantics") <- "test"
t=t+2
t=t+3
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")

setwd("output")
toFile(gRlayout , layoutType="dot", filename="miniexample.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="miniexample.svg", fileType="svg")
system(command = "dot -Tpdf miniexample.dot -o miniexpample.pdf")
setwd("../")

algebr$reset()


