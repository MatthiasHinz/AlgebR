source("graphFunctions.R")
algebr$enableProvenance()
t=1
t=t+2
t=t+3
algebr$disableProvenance()
gRlayout = algebr$getScriptGraph()
plot(gRlayout, main="Derivation Graph")
algebr$reset()

