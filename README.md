# AlgebR


## 1. Installation
### 1.1 Library dependencies
```
install.packages("codetools")
install.packages("devtools")
install.packages("stringr")
devtools::install_github('duncantl/CodeDepends')
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
```
### 1.2 Dependencies of workflow examples 
#### Interpolation example (mss package)
```
install.packages(c("xts","gstat", "sp", "devtools"))
install.packages("rgeos") #optional
devtools::install_github("edzer/mss")
```
####Spatial partitioning exammple (SPODT package)
```
install.packages("SPODT")
install.packages("tree")
```

##2. Getting started
###2.1 Getting started with a minimalistic example

After installing all packages from section 1.1. and the sp-package you should be able to execute the following script.
```
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
```
The resulting derivation graph, rendered and exported as an PNG-file, should look like the following:

![Console Session](https://github.com/MatthiasHinz/AlgebR/raw/master/output/myDerivationGraph.png)

###2.2 Getting started with the examples from GitHub
 After installation all dependencies mentioned in section 1, download or checkout the github repository. The R working has to be set to the directory where all scripts and files are contained. Now you should be able to execute any of the example-scripts, e.g. `example-mini.R`, `SPODT-example.R` or `example-interpolation_algebR.R` 
 
 All derivation graphs will be exported and written to the output-folder in different formats (currently dot, svg and pdf). Please note that the plot in R does not display the derivation grahps with all details, so always take the exported files as referenece (See known issues in section 5.1) 
 

##3. Usage

###3.1 load library

In order to load AlgebR into R you just need to download and source the file `graphFunctions.R`.
`source("graphFunctions.R") #offline`

If you want, you can also source the file directly from github, which is faster and assures you always work with the lates version
`source("https://github.com/MatthiasHinz/AlgebR/raw/master/graphFunctions.R") #directly load from gitub`

If you now inspect the workspace with `ls()`, you should have the following output, if you started a clean R session.
With exception of the `captureSemantics` functions, are functions and objects are bundled wihin the `algebr`-envrionment. The easiest way to acess them is using `algebr$...`. You can also mask the library on the global environment using `attach(algebr)`, but it is not recommended, as it will result in an overfull workspace and may cause unexpected behaviour.
```
> ls()
[1] "algebr"             "captureSemantics"   "captureSemantics<-"
```

###3.2 Provenance recording

You can start and stop provenance recording at any place in the script using the `enableProvenance` and `disableProvenance` functions:

```
 # Enter any comands you don't want to be tracked or displayed 
 # in the derivation graph outside the enable/disable-Provenance block. 
 # This could include loading libraries or defining functions used to carry out the analysis
 
algebr$enableProvenance()
  # <Your analysis you want to be tracked goes here>
algebr$disableProvenance()
 #
 
 ## export provenance and print provenance information after the analysis
```
If you want AlgebR to go back to it's state before you did any recording, use the `reset`-function. This will delete all recorded provence information including the version history of all object and the actual derivation graph. It will not delete or reset any provenance wrappers, however (see below).

`algebr$reset()`

###3.3 Render and export derivation graphs

The function `algebr$getScriptGraph()` takes the recently recorded derivation graph and returns it as an `Ragraph`-object that can be ploted using the `Rgraphviz` package. The object, as internally used represented and used in AlgebR is a nestled list of nodes, edges and graphics attributes and can be directly accessed with `algebr$scriptGraph`.

```
gRlayout = algebr$getScriptGraph()

plot(gRlayout, main="Derivation Graph")
```
It is possible to render and export the the derivation graph in various different layout types and file formats using the toFile function of the package Rgraphviz. An even greater range of rendering possiblities are given by directly passing system commands to GraphViz and converting the previously exported dot-file to the favoured format/layout. It is recommended to keep the default layout type 'dot', but feel free to explore other options such as 'neato' and 'twopi'.

For more details, please refer to the complete documentation:
 - Rgraphviz by E. Ganssner, S. North, www.graphviz.org
 - http://bioconductor.org/packages/release/bioc/html/Rgraphviz.html
```
toFile(gRlayout , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
toFile(gRlayout , layoutType="dot", filename="myDerivationGraph.svg", fileType="svg")
system(command = "dot -Tpdf myDerivationGraph.dot -o myDerivationGraph.pdf")

```
###3.3 Object versioning history

##4. Export and visualization of derivation graphs

##5. Known Issues

###5.1 Derivation graphs ploted in R
 The graphs as plotted in R does not display some line and arrow charactaristics. Also they don't display the semantic annotations at the moment. This is because the library RGraphviz does not implement all Graphics parameters supported by Graphviz
