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
toFile(gRlayout , layoutType="dot", filename="myDerivationGraph.dot", fileType="dot")
system(command = "dot -Tpng myDerivationGraph.dot -o myDerivationGraph.png")
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
```
> ls()
[1] "algebr"             "captureSemantics"   "captureSemantics<-"
```
With exception of the `captureSemantics` functions, all functions and objects are bundled wihin the `algebr`-envrionment. The easiest way to acess them is using `algebr$...`. You can also mask the library on the global environment using `attach(algebr)`, but it is not recommended, as it will result in an overfull workspace and may cause unexpected behaviour.

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
###3.4 Versioning history of variables

During an analysis, an object or variable can be overwritten or modified multiple times. When provenance tracking is enabled, AlgebR keeps a versioning history for every variable that is written to the global environment and updates it, whenever a change is noticed. The version history contains information about the 'class' and 'semantics' of the object denoted with the variable, the 'command' that caused the variable modifcation or creation and a 'timestamp', which says when this happened. Less intuitive are the attibutes 'rec_num' and 'IID': The former stands for 'record number' and denotes the number of the task or command that was recorded, starting from the first record; the latter stands for 'instance id' and denotes an identifier that is used to denote a specific instance of the variable in the versioning history. The IID is based on the variable name and also denotes nodes of the derivation graph that refer to a variable.

Subsets, like meuse$zinc for instance, also have a versioning histories if they where modified within the script. Note that  these are less reliable because subsets can be accessed an modified in different ways and it is at least very difficult to keep track of them.

The versioning history can be accessed using the `versions`-function as the following:

```
algebr$versions(meuse)
  rec_num     IID                  class semantics                                command                                  timestamp
1       1   meuse SpatialPointsDataFrame S x Q set demo(meuse, ask = FALSE, echo = FALSE) ##------ Tue Aug 16 17:59:40 2016 ------##
2       2 meuse~2 SpatialPointsDataFrame S x Q set          meuse$lzinc = log(meuse$zinc) ##------ Tue Aug 16 17:59:41 2016 ------##
```
If you only want to retrieve information about the latest recorded instance, you can use the `instance` function for convenience:
```
> algebr$instance(meuse)
  rec_num     IID                  class semantics                       command                                  timestamp
2       2 meuse~2 SpatialPointsDataFrame S x Q set meuse$lzinc = log(meuse$zinc) ##------ Tue Aug 16 17:59:41 2016 ------##
```

###3.5 Getting and setting semantics of objects

The semantics of an object can be determined using the `estimateSemantics` function as the following:
```
> algebr$estimateSemantics(meuse)
[1] "S x Q set"
```

By default, the estimateSemantics version uses a mapping from certain object properties, primarily the class, and semantic annotations. It is also possibe to annotate an object directy by setting a semantics-attribute
```
> attr(meuse,"semantics") <- "'a set"
> algebr$estimateSemantics(meuse)
[1] "'a set"
```
The semantics of an object are reflected in the white nodes of the derivation graphs

###3.6 Getting and setting semantics of function calls

#### 3.6.1 Apply semantic function wrappers

The semantics of unevaluated functions are treated the same way for object as in the previous section. However, in order to order to to cature call semantics, i.e. when a function is called/evaluated with given inputs, function wrappers have to be applied. The replacement function `captureProvenance(x) <- TRUE` puts such a wrapper around the function x. The wrapper has the same signature as the original function, thus can be executed normally. The semantics of in-and outputs are recorded internally and will be reflected in the derivation graph. However, in some cases the automated wrapping (see known issues) is not feasable. In this case it is recomended to wrap the function write a function wrapper manually and then apply the `captureProvenance` function.

#### 3.6.2 Define and check user-defined semantics

It is possible to explicitely define the semantics which are expected for a function call. This will, however, not change the behaviour of the function itself, but the user will get notified through warnings and by reviewing the derivation graph about semantic inconsistensies.

There are two ways to define the expected semantics, either as default semantics for a function wrapper within call itself, where it is only evaluated for one specific case:
  
  1) Given a function 'log' (see full definition below), it is possible to define the default semantics when applying the semantic function wrapper as the following: `captureSemantics(log, semantics=c("Q -> Q", "Q set -> Q set")) <- TRUE`. It means that log function semantics: Given a Q-value it returns another Q-value and given a Q set it returns another Q set. In this case, Q would be a number.
  To determine the default semantics of a given function, the function `callSemantics` can be used. It will return the String-vector of all allowed call semantics, NULL if the sunction is not semantics-enabled and 'dynamic' if the function has no default call semantics defined. Example: 
  ```
  > algebr$callSemantics(log)
  [1] "Q -> Q"         "Q set -> Q set" 
  ```
  
  2) If the user wants to define semantics for a given semantics-enabled function 'log' only for one call, it can be done as the following by motifying the actual function call: `log(t, semantics = c("Q set -> Q set"))`. If the semantics function wrapper has defined default semantics, they will be ignored for this call.

The following scipt demonstrates how to define the expected call semantics and how semantic inconsistencies are handled. The resulting derivation graph is displayed below the script.

```
source("https://github.com/MatthiasHinz/AlgebR/raw/master/graphFunctions.R")
log = function(x){
  return(base::log(x))
}
captureSemantics(log, semantics = c("Q -> Q", "Q set -> Q set")) <- TRUE

algebr$enableProvenance()
t=123
log(t) #no inconsistencies/warnings
log(t, semantics = c("Q set -> Q set")) #warning, because the call semantics are Q -> Q, which is not expected
attr(t, "semantics") <-"myType"
log(t) #warning, because the call semantics are myType -> myType, which is not expected
algebr$disableProvenance()

gRlayout = algebr$getScriptGraph()
toFile(gRlayout , layoutType="dot", filename="inconsistentGraph.dot", fileType="dot")
system(command = "dot -Tpng inconsistentGraph.dot -o inconsistentGraph.png")
```

![Console Session](https://github.com/MatthiasHinz/AlgebR/raw/master/output/inconsistentGraph.png)

##4. External visualization of derivation graphs

Section 3.3. describes briefly how to export derivation graphs from R in different formats like pdf, svg and png. One output format that deserves more attention is the 'dot' or 'gv' format, which is native to the Graphviz software (http://www.graphviz.org/). A list of available for software for this format is available at http://www.graphviz.org/content/resources

For Ubuntu users, it is practical to install the XDOT app, which allows viewing and navigating through graphs by zooming/panning. However, it does not implement editing or layout functionalities.
https://apps.ubuntu.com/cat/applications/xdot/

It is possible to import dot files to Cytoscape, (http://www.cytoscape.org/), which is an platform for comlpex network analysis and visualization that was originally designed for biological research. This requires installing cytoscape with the dot-app (http://apps.cytoscape.org/apps/dotapp). Unfortunally the dot-app has some limitations (see http://chianti.ucsd.edu/dot-documents/conversion_details.pdf)

##5. Known Issues

###5.1 Derivation graphs ploted in R
 The graphs as plotted in R does not display some line and arrow charactaristics. Also they don't display the semantic annotations at the moment. This is because the library RGraphviz does not implement all Graphics parameters supported by Graphviz
 
###5.2 CodeDepends/mss - incompatiblity
The script example_src_blockKriging.R throws errors when using the [/[[-operator on some objects. It seem like the reason is an incompatiblity between the packages 'CodeDepends' and 'mss'.

```
        > zincBlockData = zincBlockData[!is.na(zincBlockData[[1]]),]
            Fehler in x@observations[i, j, ..., drop = drop] :
              Argument "j" fehlt (ohne Standardwert)
```
###5.3 CaptureProvenace function
In some cases, applying a semantics wrapper with the `captureProvenance` function does not work as expected: Primitive R functions are not yet supported for different reasons. Some functions wrappers throw unexpected errors when called, in particular those which have extraordinary inputs such as the ... - parameter. Exact reasons still have to be examined. A workaround is to write manually a wrapper a wrapper around the function using the arguments you want to pass and then apply the `captureProvenance` function.
