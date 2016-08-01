#library(stringr)

algebr = new.env()


####
# Provenance functions
####

#initiates a new graph opject for recording
algebr$newDerivationGraph <-function(){
  g = list(V = c(), E = list(), eAttrs=list(), nAttrs=list(), attrs=list(), fCalls=list())
  g$attrs <- list(node=list(shape="ellipse", fixedsize=FALSE, fillcolor="white", style="filled"))
  return(g)
}

# returns derivation graph of the script in Ragraph format (Rgraphviz)
# can also convert other graphs created by algebr$newDerivationGraph to the Ragraph format (g-parameter)
algebr$getScriptGraph <- function(g=algebr$scriptGraph){
  gR <- graphNEL(nodes = g$V,
                 edgeL = g$E,
                 edgemode = "directed")
  graph.par(list(fontsize=14))
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
    algebr$history_list = append(algebr$history_list, expr)
    #cat(as.character(as.expression(expr)))
    new_ls = ls(envir = globalenv())
    
    #notify and track new variables
    new_vars = new_ls[!new_ls %in% algebr$last_ls]
    if(length(new_vars)>0)
      cat(paste("The following variables have been initialized: ", paste(new_vars, collapse = " ")))
    
    ls(envir = globalenv())[ls() %in% algebr$ls_last]
    
    info = scriptInfo(readScript(txt=as.character(as.expression(expr))))
    side_effects = new_vars[!new_vars %in% info[[1]]@outputs]
    if(length(side_effects)>0)
    warning(paste("These variables have been initialized from side-efects of the previous task: ", paste(side_effects, collapse = " ")))
    #look
    
    # actually parsing the last executed expression to a graph:
    algebr$scriptGraph=algebr$parseCommand(expr,algebr$scriptGraph)
    
    algebr$last_ls = new_ls
    return(TRUE)
  }
}


algebr$enableProvenance <- function(){
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
  for(variable_str in algebr$last_ls){
    #variable = get(variable_str)
    isTracked=eval(parse(text=paste0("attr(",variable_str,", \"isTracked\")")), envir=globalenv())
    
    if(!isTRUE(isTracked)){
      isTracked=eval(parse(text=paste0("attr(",variable_str,", \"isTracked\") <- FALSE")), envir=globalenv())
    }
  }
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
  algebr$history_list = list()
  algebr$scriptGraph = newDerivationGraph();
}

###
# Functions for creating a derivation graphs
####

algebr$addNodeObject <- function(var, g) {
  var = as.character(as.expression(var))
  if (all(g$V != var)){
    g$V = append(g$V, var)
    g$E[[var]]=list(edges=c(), weights=c())
  }
  g$last_vt=var
  g
}


algebr$addEdgeOutput <- function(output, cmd, g) {
  if(all(g$E[[cmd]]$edges != output)){
    g$E[[cmd]]$edges = append(g$E[[cmd]]$edges, output)
    g$E[[cmd]]$weights = append(g$E[[cmd]]$weights, 1)
    g$eAttrs$color[[paste0(cmd, "~", output)]] = "red"
  }
  return(g)
}

algebr$addEdgeInput <- function(input, cmd, g, label=NULL){
  if(all(g$E[[input]]$edges != cmd)){
    g$E[[input]]$edges = append(g$E[[input]]$edges, cmd)
    g$E[[input]]$weights = append(g$E[[input]]$weights, 1)
    if(!is.null(label)){
      g$eAttrs$label[[paste0(input,"~",cmd)]] =label
    }
  }else
    warning(paste("AlgebR: Dublicate edge from", input,"to",cmd,"! Second edge (and posdibly the label) will not display in derivation graph."))
  return(g)
}





algebr$parseCommand = function(cmd, g=list(V = c(), E = list(), attrs=list(), eAttrs=list(), nAttrs=list(), chunks=list(), last_vt=NULL)){
  g$first_call = NULL
  
  #print(paste(as.character(as.expression(cmd)), class(cmd)))
  cmd_id = NULL
  #CASE 1: cmd is some kind of variable
  if(is.name(cmd)){
    g=algebr$addNodeObject(cmd, g)
    cmd_id=g$last_vt
    #CASE 2: cmd is some kind of literal
  }else if(any(sapply(list(is.numeric, is.symbol, is.character, is.logical), function(x){x(cmd)}))){
    #print("---------------------")
    #print(paste0("found literal: ",cmd))
   # print("---------------------")
    cmd_id=as.character(as.integer(runif(1)*1000))
    g$nAttrs$label[[cmd_id]]=as.character(cmd)
    g$V = append(g$V, cmd_id)
    #CASE 2: cmd is an assignment (TODO: handle operators like ->, <<- ...)
  }else if(any(class(cmd) ==c("=", "<-"))){
    # handle left-hand side of assignmet
    #print(paste("something wrong? - sould be an assignment",as.character(as.expression(cmd))))
    g = algebr$parseCommand(cmd[[2]], g)
    output_id=g$last_vt
    #---- Parses right-hand side of the assignment:
    g = algebr$parseCommand(cmd[[3]], g)
    #--------------------------------------------------------------
    cmd_id = g$last_vt
    g=algebr$addEdgeOutput(output = output_id, cmd = cmd_id, g)
    #case 3: cmd is some kind of call, (possibly right hand of an assignment) 
  } else if(class(cmd)=="call"){
    
    #Case 3.1: cmd is a function definition (whole definition can hardly be displayed or has to be parsed with special care)
    if(cmd[[1]]=='function'){
      cmd_id=paste0("def",as.character(as.integer(runif(1)*1000)))
      # cmd_o=paste(as.character(as.expression(cmd), collapse="\n", sep=""))
      g$chunks[[cmd_id]]=list(code=cmd, id=cmd_id)
      g$nAttrs$fillcolor[[cmd_id]]="orange"
      g$V = append(g$V, cmd_id)
      g$E[[cmd_id]]=list(edges=c(), weights=c())
      #handle function definitions
      
    }else if(cmd[[1]]=='['|| cmd[[1]]=='$'|| cmd[[1]]=='@'){
      reference = cmd #TODO: add (this) object reference to profenance of this node
      
      parseSelection = function(cmd){
        #print(paste(as.character(as.expression(cmd)), "parseSelection"))
        parent = cmd[[2]]
        selection = paste(cmd[3:length(cmd)],collapse =";")
        
        if(length(parent)>1 && (parent[[1]]=='['|| parent[[1]]=='$'|| parent[[1]]=='@')){
          subselection = parseSelection(cmd[[2]])
          parent = subselection$parent
          selection = paste(subselection$selection, selection, sep="_")
          return(list(parent = parent, selection=selection))
        }else{
          return(list(parent = parent, selection=paste(parent, selection, sep="_")))
        }
      }
      
      selout=parseSelection(cmd)
     # print(paste("OUT:", selout$parent, selout$selection))
      g=algebr$parseCommand(selout$parent,g)
      parent = g$last_vt
      
      g=algebr$parseCommand(as.name(selout$selection),g)
      query = g$last_vt
      
     # print(paste(parent, selout$selection," <- create selection"))
      g=algebr$addEdgeInput(parent, query, g)
      cmd_id=g$last_vt
      #-------------------
    }else if(any(cmd[[1]]==c("log","sin","cos"))&& (is.character(cmd[[2]]) || is.numeric(cmd[[2]]))){
      g = algebr$parseCommand(as.character(as.expression(cmd)));
      query=g$last_vt
      g$nAttrs$fillcolor[[query]]="orange"
      cmd_id=g$last_vt
      #------------------------------------
      g = algebr$parseCommand(cmd[[2]], g)
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
        label=as.character(as.expression(cmd[[1]]))
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
            g = algebr$parseCommand(arg, g)
            # connect operand/arguments as input to the operator/function:
            label=names(cmd[i])
            g=algebr$addEdgeInput(g$last_vt, cmd_id, g, label)
          }
        
      }
    
    #for all calls:
    for(V in g$V){
        cmd_ids = as.character(cmd_id)
        call_label=g$fCalls[[cmd_ids]]
        if(!is.null(call_label) && V==call_label){

          if(all(g$E[[V]]$edges != cmd_ids)){
            g$E[[V]]$edges = append(g$E[[V]]$edges, cmd_ids)
            g$E[[V]]$weights = append(g$E[[V]]$weights, 1)
            g$eAttrs$color[[paste0(V, "~", cmd_ids)]] = "blue"
          }
      }
      
    }
    
    g$first_call=cmd_id
  }
  
  
  g$last_vt=cmd_id
  return(g)
}



###
# Utility functions
####


algebr$getChunks = function(g, cmd){
  return(g$chunks[[as.character(cmd)]]$code)
}



# visualy compare if each vertex of a graph has a matching list of edges (required for GraphViz)
algebr$compareCE = function(g){
  print(sort(g$V))
  print(sort(names(g$E)))
}





