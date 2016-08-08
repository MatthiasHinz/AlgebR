library(stringr)
library("Rgraphviz")
library(CodeDepends)
library(codetools)


algebr = new.env()


####
# Provenance functions
####

#initiates a new graph opject for recording
algebr$newDerivationGraph <-function(){
  g = list(V = c(), E = list(), eAttrs=list(), nAttrs=list(), attrs=list(), fCalls=list())
  g$attrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fillcolor="white", style="filled"),
                  edge=list(style="solid", arrowhead="normal"))
  return(g)
}

# returns derivation graph of the script in Ragraph format (Rgraphviz)
# can also convert other graphs created by algebr$newDerivationGraph to the Ragraph format (g-parameter)
algebr$getScriptGraph <- function(g=algebr$scriptGraph){
  gR <- graphNEL(nodes = g$V,
                 edgeL = g$E,
                 edgemode = "directed")
  graph.par(list(fontsize=11))
  gR <- layoutGraph(gR) #graphNEL format (graph package, just edges and nodes)
  #Ragraph format (GraphViz package, includes layout information)
  gRlayout <- agopen(gR, name="f", attrs=g$attrs, nodeAttrs=g$nAttrs, edgeAttrs=g$eAttrs) 
}

algebr$provenanceCallback <- function(algebr_env = algebr) {
  isFirstCall = TRUE 
  function(expr, value, ok, visible, data=algebr_env) {
    if(isFirstCall){
      #don't track first task executed (= call to enableProvenance() )
      isFirstCall <<- FALSE
      return(TRUE)
    }
    algebr=data
    algebr$rec_num = algebr$rec_num+1 #record number
    #------------------------------------------------------------------------------------
    # Collect provenance information from workspace changes that may be used for parsing
    #------------------------------------------------------------------------------------
    algebr$history_list = append(algebr$history_list, expr)
    #cat(as.character(as.expression(expr)))
    algebr$new_ls = ls(envir = globalenv())
    
    #notify and track new variables
    new_vars = algebr$new_ls[!algebr$new_ls %in% algebr$last_ls]
   # if(length(new_vars)>0)
   #   cat(paste("The following variables have been initialized: ", paste(new_vars, collapse = " "),"\n"))
    
    ls(envir = globalenv())[ls() %in% algebr$ls_last]
    
    info = scriptInfo(readScript(txt=as.character(as.expression(expr))))
    side_effects = new_vars[!new_vars %in% info[[1]]@outputs]
    
    
    #------------------------------------------------------------------------------------
    # Parse available provenance information to enhance the derivation graph
    #------------------------------------------------------------------------------------
    
    # actually parsing the last executed expression to a graph:
    algebr$scriptGraph=algebr$parseCommand(expr,algebr$scriptGraph, first_call = TRUE)
    

    if(length(side_effects)>0){
      cmd_id = algebr$scriptGraph$first_call
      sapply(side_effects, function(variable){
        algebr$scriptGraph <<- algebr$addNodeObject(var = variable, g = algebr$scriptGraph, isOutput = TRUE)
        algebr$scriptGraph <<- algebr$addEdgeOutput(output =  variable,cmd = cmd_id,g = algebr$scriptGraph, hidden = TRUE)
      }
        
      )
    # warning(paste("These variables have been initialized from side-efects of the previous task: ", paste(side_effects, collapse = " ")))
    }#look
    #-----------------------------------------------------------------------------------

    # Be aware that the last_ls variable is overwritten IN THE END of the callback, but may be used during parsing from various methods
    # So please don't move it!
    algebr$last_ls = algebr$new_ls
    return(TRUE)
  }
}


algebr$enableProvenance <- function(){
  if(is.null(algebr$rec_num))
    algebr$rec_num = 0
  if(is.null(algebr$version_history))
    algebr$version_history = list()
  if(is.null(algebr$history_list))
    algebr$history_list = list()
  if(is.null(algebr$scriptGraph))
    algebr$scriptGraph = algebr$newDerivationGraph()
  
  
  if(!isTRUE(algebr$isEnabled)){
    algebr$callback <- addTaskCallback(algebr$provenanceCallback())
    algebr$isEnabled = TRUE
  }else{
    warning("Provenance tracking is already enabled!")
    return(invisible())
  }
  
   algebr$last_ls = ls(envir = globalenv())
   
  # for(variable_str in algebr$last_ls){
  #   #variable = get(variable_str)
  #   isTracked=eval(parse(text=paste0("attr(",variable_str,", \"isTracked\")")), envir=globalenv())
  #   
  #   if(!isTRUE(isTracked)){
  #     isTracked=eval(parse(text=paste0("attr(",variable_str,", \"isTracked\") <- FALSE")), envir=globalenv())
  #   }
  # }
  invisible()
}

algebr$disableProvenance <-function(){
  
  if(isTRUE(algebr$isEnabled)){
    algebr$isEnabled= FALSE
    removeTaskCallback(algebr$callback)
  }else{
    warning("Provenance tracking is already disabled!")
  }
  invisible()
}




algebr$history <- function(){
  return(algebr$history_list)
}

algebr$reset <-function(){
  algebr$rec_num = 0
  algebr$version_history = list()
  algebr$history_list = list()
  algebr$scriptGraph = algebr$newDerivationGraph();
}

###
# Functions for creating a derivation graphs
####

algebr$addNode = function(node_id, g, label=NULL, color=NULL, shape=NULL){
  node_id = algebr$unquote(as.character(as.expression(node_id)))
  if (all(g$V != node_id)){
    g$V = append(g$V, node_id)
    g$E[[node_id]]=list(edges=c(), weights=c())
  }
  
  if(!is.null(label))
    g$nAttrs$label[[node_id]]=label
  
  g$last_vt=node_id
  return(g)
}

#returns the last instance of the version history of a variable, i.e. the most recently collected metadata
algebr$instance <- function(var){
  if(!is.character(var))
    var=as.character(substitute(var))
   versions = algebr$versions(var)
   return(versions[dim(versions)[1],])
}

algebr$addNewVersionRecord <- function(var){
  var=as.character(as.expression(var))
 # print(paste("Version update of", var))
  if(is.null(algebr$version_history[[var]]))
    algebr$version_history[[var]]=data.frame()
  #make sure that versions updated only ONCE per execution (even if this method might be called multiple times)
  else if(algebr$instance(var)$rec_num>=algebr$rec_num){
    return()
  }
  
  instance=data.frame(rec_num = algebr$rec_num, class=class(eval(parse(text=var),envir = globalenv())), semantics = algebr$estimateSemantics(var), timestamp=timestamp(quiet = TRUE))
  algebr$version_history[[var]]=rbind(algebr$version_history[[var]], instance)
}

algebr$versions <- function(var){
  if(!is.character(var))
    var=as.character(substitute(var))
  #print(var)
  return(algebr$version_history[[var]])
}

algebr$addNodeObject <- function(var, g, isInput=FALSE, isOutput=FALSE, isSubset=FALSE) {
  
  var = algebr$unquote(as.character(as.expression(var)))#ensure that variable name is a string
  #print(paste("addN:",var))
  isVersionUpdated=FALSE
  label=var
  node_name=var
  if((isSubset || exists(var,envir = globalenv())) && is.null(algebr$version_history[[var]])){
    algebr$addNewVersionRecord(var)
    isVersionUpdated=TRUE
  }
  
  if(isInput && isOutput)
    #normally never happens...
    warning(paste0("Algebr: The variable \"",var,"\" was classified as Input AND output. The resulting derivation graph may be flawed."))
  
  if(isInput){
    #verify that object existed before the last task was executed
    #if not, it shall be treaded as a litteral, e.g. value of class 'symbol' or 'expression'
    #IMPORTANT: this test is heuristic, because the workspace cannot (yet?) really be examined before execution
    # -- so there might be cases of missclassification as literal
    
    #if-clause assumes that the previously executed task did not add variables to any parent environment
    if(!var %in% algebr$last_ls && !isSubset && !exists(var, envir = parent.env(globalenv()))){
        return(algebr$addNodeLiteral(label = var, g))
    }
    ver_num = dim(subset(algebr$versions(var), rec_num<algebr$rec_num))[1]
    #versioning support of variables
   # print(paste(ver_num, "version_num1",var, algebr$rec_num))
    if(ver_num>1){
      node_name = paste0(var,"~",ver_num)
      label=node_name
    }
    class=algebr$versions(var)[ver_num, "semantics"]
    #print(paste("CLASS,",class))
    if(length(class)==0 &&isSubset)
      class=algebr$versions(var)[ver_num+1, "semantics"]
    label=paste0(label, " \\n[",class,"]")
  }else if(isOutput){
    #for outputs, its sufficient to check if objects exists in current workspace
    if(!var %in% algebr$new_ls && !isSubset && !exists(var, envir = parent.env(globalenv()))){
      return(algebr$addNodeLiteral(label = var, g))
    }
    #create entry in version history
    if((isSubset || exists(var,envir = globalenv())) && !isVersionUpdated){
      algebr$addNewVersionRecord(var)
    }
    
    #versioning support of variables
  
    ver_num = dim(algebr$versions(var))[1]
    #print(paste(ver_num, "version_num2",var, algebr$rec_num))
    if(ver_num>1){
      node_name = paste0(var,"~",ver_num)
      label=node_name
    }
    label=paste0(label, " \\n[",algebr$versions(var)[ver_num, "semantics"],"]")
  }
  
  g=algebr$addNode(node_name,g, label = label)
  return(g)
}


algebr$addEdgeOutput <- function(output, cmd, g, hidden=FALSE) {
  if(all(g$E[[cmd]]$edges != output)){
    g$E[[cmd]]$edges = append(g$E[[cmd]]$edges, output)
    g$E[[cmd]]$weights = append(g$E[[cmd]]$weights, 1)
    g$eAttrs$color[[paste0(cmd, "~", output)]] = "red"
    if(hidden)
      g$eAttrs$style[[paste0(cmd, "~", output)]] = "dashed"
  }
  return(g)
}

algebr$addEdgeInput <- function(input, cmd, g, label=NULL, hidden=FALSE){
  if(all(g$E[[input]]$edges != cmd)){
    g$E[[input]]$edges = append(g$E[[input]]$edges, cmd)
    g$E[[input]]$weights = append(g$E[[input]]$weights, 1)
    if(!is.null(label)){
      g$eAttrs$label[[paste0(input,"~",cmd)]] =label
    }
    g$eAttrs$arrowhead[[paste0(input, "~", cmd)]] = "onormal"
    
    if(hidden)
      g$eAttrs$style[[paste0(input, "~", cmd)]] = "dashed"
  }else
    warning(paste("AlgebR: Dublicate edge from", input,"to",cmd,"! Second edge (and possibly the label) will not display in derivation graph."))
    g$last_vt = input
  return(g)
}


algebr$addEdgeFunctionCall <- function(fun_id, call_id, g, hidden=FALSE){
  if(all(g$E[[fun_id]]$edges != call_id)){
    g$E[[fun_id]]$edges = append(g$E[[fun_id]]$edges, call_id)
    g$E[[fun_id]]$weights = append(g$E[[fun_id]]$weights, 1)
    g$eAttrs$color[[paste0(fun_id, "~", call_id)]] = "blue"
    if(hidden){
      g$eAttrs$style[[paste0(fun_id, "~", call_id)]] = "dashed"
    }
  }
  return(g)
}


algebr$addNodeLiteral = function(label, g){
  vt_id=as.character(as.integer(runif(1)*100000)) #generates a random id for the node to be unique
  g=algebr$addNode(node = vt_id, g = g,label = label)
  
  
  return(g)
  #g$V = append(g$V, cmd_id)
}

algebr$parseCommand = function(cmd, g=list(V = c(), E = list(), attrs=list(), eAttrs=list(), nAttrs=list(), chunks=list(), last_vt=NULL), first_call=FALSE, isInput=FALSE, isOutput=FALSE){
  if(first_call)
    g$first_call = NULL
  
  #print(paste(as.character(as.expression(cmd)), class(cmd)))
  cmd_id = NULL
  #CASE 1: cmd is some kind of variable
  if(is.name(cmd)){
    g=algebr$addNodeObject(cmd, g, isInput= isInput, isOutput=isOutput)
    cmd_id=g$last_vt
    #CASE 2: cmd is some kind of literal
  }else if(any(sapply(list(is.numeric, is.symbol, is.character, is.logical), function(x){x(cmd)}))){
    #print("---------------------")
    #print(paste0("found literal: ",cmd))
    # print("---------------------")
    g=algebr$addNodeLiteral(as.character(as.expression(cmd)), g)
    cmd_id=g$last_vt
    #CASE 2: cmd is an assignment (TODO: handle operators like ->, <<- ...)
  }else if(any(class(cmd) ==c("=", "<-", "<<-"))){
    cmd = algebr$rewriteReplacementFunction(cmd) #turn calls to replacement functions into a parser-friendly style
    # handle left-hand side of assignmet
    #print(paste("something wrong? - sould be an assignment",as.character(as.expression(cmd))))
    g = algebr$parseCommand(cmd[[2]], g, isOutput=TRUE)
    output_id=g$last_vt
    #---- Parses right-hand side of the assignment:
    g = algebr$parseCommand(cmd[[3]], g, isInput=TRUE)
    #--------------------------------------------------------------
    cmd_id = g$last_vt
    g=algebr$addEdgeOutput(output = output_id, cmd = cmd_id, g)
    #case 3: cmd is some kind of call, (possibly right hand of an assignment) 
  } else if(class(cmd)=="call"){
    
    #Case 3.1: cmd is a function definition (whole definition can hardly be displayed or has to be parsed with special care)
    if(cmd[[1]]=='function'){
      cmd_id=paste0("def",as.character(as.integer(runif(1)*100000)))
      # cmd_o=paste(as.character(as.expression(cmd), collapse="\n", sep=""))
      g$chunks[[cmd_id]]=list(code=cmd, id=cmd_id)
      g$nAttrs$fillcolor[[cmd_id]]="orange"
      g$V = append(g$V, cmd_id)
      g$E[[cmd_id]]=list(edges=c(), weights=c())
      #handle function definitions
      
    }else if(cmd[[1]] =='[[' || cmd[[1]]=='['|| cmd[[1]]=='$'|| cmd[[1]]=='@'){
      reference = cmd #TODO: add (this) object reference to profenance of this node
      
      findParent <- function(cmd){
        if(length(cmd)==1)
          return(cmd)
        else
          return(findParent(cmd[[2]]))
      }
      
      parent = findParent(cmd)
      #print(paste("Parent: ",parent))
      g=algebr$addNodeObject(var = parent, g = g, isInput = isInput,isOutput = isOutput)
      parent = g$last_vt
      
      #g=algebr$parseCommand(as.name(selout$selection),g, isInput = FALSE, isOutput = FALSE) ##TODO not so clear how to handel isInput/isOutput flags here (may lead to misclassification in addNodeObject currently)
      g=algebr$addNodeObject(var = cmd, g = g, isInput = isInput,isOutput = isOutput, isSubset=TRUE)
      query = g$last_vt
      cmd_id=g$last_vt
      # print(paste(parent, selout$selection," <- create selection"))
      if(isInput){
        g=algebr$addEdgeInput(parent, query, g)
      }else if(isOutput){
        g=algebr$addEdgeOutput(parent, query, g)
      }
      #-------------------
    }else if(any(cmd[[1]]==c("log","sin","cos"))&& (is.character(cmd[[2]]) || is.numeric(cmd[[2]]))){
      g = algebr$parseCommand(as.character(as.expression(cmd)));
      query=g$last_vt
      g$nAttrs$fillcolor[[query]]="orange"
      cmd_id=g$last_vt
      #------------------------------------
      g = algebr$parseCommand(cmd[[2]], g, isInput=TRUE)
      #-------------------------------------
      g=algebr$addEdgeInput(g$last_vt, query, g)
      #-------------------
      
      #Case 3.2: cmd is a function call or some operation (e.g. mathematical, logical)
    }else
      # print(paste("found call: ",as.character(as.expression(cmd))))
      ##TODO: preserve mathematical expressions in one node, 
      #i.e. diferentiate between mathematical/logical operations and function calls
      if(eval(call("is.function", cmd[[1]]))){
        cmd_id=as.character(as.integer(runif(1)*1000))
        label=algebr$unquote(as.character(as.expression(cmd[[1]])))
        # print(paste("label: ",label))
        g$fCalls[[cmd_id]]=label
        fdef=eval(cmd[[1]])
        if(!is.primitive(fdef))
          cmd=match.call(fdef, cmd)
        
        #print(cmd[2:length(cmd)])
        #print(paste0("label: ", label))
        g$nAttrs$label[[cmd_id]]=label
        g$nAttrs$fillcolor[[cmd_id]]="orange"
        g$nAttrs$style[[cmd_id]]="filled"
        g$V = append(g$V, cmd_id)
        g$E[[cmd_id]]=list(edges=c(), weights=c())
        
        
        if(length(cmd)>1)
          for(i in 2:length(cmd)){
            # print(paste("cmd:",cmd, length(cmd)))
            arg=cmd[[i]]
            #------------------------------------
            g = algebr$parseCommand(arg, g, isInput=TRUE, isOutput=FALSE)
            # connect operand/arguments as input to the operator/function:
            label=names(cmd[i])
            g=algebr$addEdgeInput(g$last_vt, cmd_id, g, label)
          }

        #print(paste("findglobals...", cmd[[1]]))        
        if(!is.primitive(get(algebr$unquote(as.character(as.expression(cmd[[1]])))))){
          globals = eval(call("findGlobals", eval(parse(text=as.character(as.expression(cmd[[1]])))), merge=FALSE))
          ##TODO: expore function references to other packages
          
          ls_func = globals$functions[globals$functions %in% ls(envir = globalenv())]
          if(length(ls_func)>0)
            sapply(ls_func, function(x){
              g <<- algebr$addEdgeFunctionCall(fun_id = x,call_id = cmd_id,g = g, hidden = TRUE)
            })
          ls_vars = globals$variables[globals$variables %in% ls(envir = globalenv())]
          if(length(ls_vars)>0)
            sapply(ls_vars, function(x){
              #TODO: detect (with scriptInfo of CodeDepends ?) if global variable is updated
              g <<- algebr$addEdgeInput(input = x, cmd = call_id, g=g, hidden=TRUE)
            })}
      }
    #for all calls:
    for(V in g$V){
      cmd_ids = as.character(cmd_id)
      call_label=g$fCalls[[cmd_ids]]
      if(!is.null(call_label) && V==call_label){
        g=algebr$addEdgeFunctionCall(V, cmd_ids, g)
      }
    }
    
    if(is.null(g$first_call))
      g$first_call=cmd_id
  }
  
  
  g$last_vt=cmd_id
  return(g)
}


# turns calls of replacement functions into a parser-friendly form
#
# For instance:
# attr(t, "semantics") <- "test"
# into
# t <- `attr<-`(t, "semantics", "test")
#
# See http://adv-r.had.co.nz/Functions.html
algebr$rewriteReplacementFunction = function(expr){
  if(is.call(expr[[2]])){
    fname= expr[[2]][[1]]

    #exception for these operators:
    if(any(as.character(fname) %in% c('[[','[','$','@'))){
      print(expr)
      return(expr)
    }
      
    fname=as.name(paste0(fname,"<-"))
    # find out if a replacement function was used:
    if(exists(as.character(fname)) && eval(call("is.function", fname))){
      op = expr[[1]]
      value = expr[[3]]
      obj = expr[[2]][[2]]
      args= as.list(expr[[2]][-1]);args
      cl=append(fname, args);cl
      cl=append(cl,value);cl
      cl=as.call(cl);cl
      cl=list(op, obj, cl);cl
      cl=as.call(cl);cl
      return(cl)
    }
  }
  return(expr)
}

###
# Semantics related functions
####

algebr$estimateSemantics <- function(var, env=globalenv()){
  
  if(!is.character(var))
    var=as.character(substitute(var))
  
  print(var)
  
  #obj = get(var, envir = env)
  obj = eval(parse(text=var),envir = env)
  
  if(!is.null(attr(obj, "semantics")))
    return(attr(obj, "semantics"))
  # test if object is annotated with semantics, if not, predict semantics from class and structure (note that the latter is only a generic assumption, i.e. based on heuristics)
  # be carful about the order of if-statements, because some classes extend others but imply different semantics
  if (any(sapply(list(is.numeric, is.character, is.factor, is.symbol, is.name, is.expression), function(fun) {
    return(fun(obj))
  }))) {
    if(length(obj)<=1)
      return("Q")
    else return("Q set")
  }
  
  if (is.logical(obj)) {
    if(length(obj)<=1)
      return("bool")
    else return("bool set")
  }
  
  if (is(obj, "CRS")) { ## for the actual mss package
    return("'a")
  }
  
  if (is(obj, "SField")) { ## for the actual mss package
    obs = slot(obj, "observations")
    sObs = algebr$estimateSemantics(obs, env = environment())
    sObs = str_replace(sObs, " set","")
    return(paste(sObs, "x SExtend set"))
  }
  
  if (is(obj, "SpatialLinesDataFrame")) {
    return("S x Q set")
  }
  
  if (is(obj, "SpatialLines")) {
    return("S set")
  }
  
  
  if (is(obj, "SpatialPixelsDataFrame")) {
    return("S x Q set")
  }
  
  if (is(obj, "SpatialPointsDataFrame")) {
    return("S x Q set")
  }
  
  if (is(obj, "SpatialPixels")) {
    return("S set")
  }
  
  if (is(obj, "SpatialPoints")) {
    #how many points
    return("S set")
  }
  
  if (is(obj, "SpatialMultiPointsDataFrame")) {
    #how many points
    return("S x Q set")
  }
  
  if (is(obj, "SpatialMultiPoints")) {
    #how many points
    return("S set")
  }
  
  if (is(obj, "SpatialGridDataFrame")) {
    return("R x Q set")
  }
  
  if (is(obj, "SpatialGrid")) {
    return("R set")
  }
  
  if (is(obj, "SpatialPolygonsDataFrame")) {
    #TODO: how many Polygons?
    return("R x Q set")
  }
  
  if (is(obj, "SpatialPolygons")) {
    #TODO: how many Polygons?
    return("R set")
  }
  
  if (is(obj, "Spatial")) {
    return("S set")
  }
  
 if(any(sapply(list(is.data.frame, is.list, is.array, is.matrix), function(fun) {
    return(fun(obj))
  }))) {
      return("Q set")
  }
  
  return(class(obj))
}
#jars of clay - frail

###
# Utility functions
####

algebr$unquote = function(str){
  if(str_detect(str,"^`.*`\\Z"))
     return(str_sub(str, 2,-2))
  else
    return(str)
}


algebr$getChunks = function(g, cmd){
  return(g$chunks[[as.character(cmd)]]$code)
}



# visualy compare if each vertex of a graph has a matching list of edges (required for GraphViz)
algebr$compareVE = function(g){
  out=list(nodes=sort(g$V), edges_names= sort(names(g$E)))
  sel=which(!out$nodes %in% out$edges_names)
  print(out$nodes %in% out$edges_names)
  if(length(sel)>0)
    cat(paste("Error: These nodes are not matched by the lists of edges: ",paste(out$nodes[sel], collapse = ", ")))
  sel=which(!out$edges_names %in% out$nodes)
  print(out$edges_names %in% out$nodes)
  if(length(sel)>0)
    cat(paste("Error: These nodes do not occure the lists of nodes: ",paste(out$edge_names[sel], collapse=", ")))
  cat(paste())
  cat(paste("Lists of edges: ",length(out$edges_names),"\n"))
  cat(paste("Number of vertexes: ",length(out$nodes),"\n"))
  
  
  return(out)
}





