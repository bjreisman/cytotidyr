#' A simple wrapper for the gating functions in flowCore
#'
#' @param flow_frame A flowframe containing the data to be gated
#' @param population Desired Population ("character")
#' @param poplist Population list 
#' @param gatelist returned by `get_gates``
#' @param gate_def, returned by `define_gates`
#' @param lut, retruned by `get_expinfo`
#' @return A flow frame containing the data which lie within the selected gate
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @export
#' @import flowCore

gate_population_rev <- function(flow_frame,
                            population,
                            poplist,
                            gatelist,
                            gate_defs,
                            lut, 
                            fcsFile_lut = NULL) { #need to pass flow_frame (fcb flowCore file), gatelist (get_gates), population name (string), poplist (get_populations), lut
  # fcsFile_lut <- fcs.file.tibble
  # flow_frame <- ff.i
  # population <- populations$name[[2]]
  # poplist <- populations
  # gatelist <- gates.raw
  # gate_defs <- gates.defined
  
  pop.gates <- unlist(poplist[[match(population, poplist$name), "definition"]][["gates"]]) # gets the sequence of gates (numeric ids) that defines a population

  gate.lut <- tibble(
    gateID =   names(gate_defs),
    gateNum = as.numeric(lapply(gate_defs, `[[`, 1)),
    tailored = as.logical(lapply(gate_defs, `[[`, 3)),
    id = as.character(lapply(gate_defs, `[[`, 5))
  )
  
  gate.variant.count <- gate.lut %>%
    dplyr::filter(gateNum %in% pop.gates) %>%
    group_by(gateNum) %>%
    summarise(n = n())
  
  if(any(gate.variant.count[,"n"] != 1) & is.null(fcsFile_lut)) {
    stop("Attempted to apply tailored gate without providing an FCS file list.")
  } else{
    gate.lut <- gate.lut %>%
      left_join(fcsFile_lut %>% dplyr::select(id, filename), by = 'id')
  }
  
  #extract the filename from the flowframe
  ff_desc <- description(flow_frame)
  fcs.filename <- ff_desc$`$FIL`

  for(j in pop.gates) { #for gate in pop_gates
    gate.variant.lut <- gate.lut %>%
      dplyr::filter(gateNum == j)
    #print(gate.variant.lut)
    #checks if there exists a tailored gate for the flowframe
    if(nrow(gate.variant.lut) == 1){
      #not a tailored gate, returns gate.i
      gate.i <- gate_defs[[unlist(gate.variant.lut[1,"gateID"])]]
      print('non tailored gate')
    } else if(fcs.filename %in% gate.variant.lut$filename) { 
      #retruns tailored verison of the gate
      gateID.i <- gate.variant.lut %>%
        dplyr::filter(filename == fcs.filename)
      gate.i <- gate_defs[[gateID.i$gateID]]
      print('tailored gate')
      
    } else { #uses the non-tailored version
      gateID.i <- gate.variant.lut %>%
        dplyr::filter(is.na(filename))
      gate.i <- gate_defs[[gateID.i$gateID]]
      print('tailored gate, base gate')
      
    }
    
    #i<- j
    axis <- as.formula(paste0("`",gate.i[["channels"]][2],"`", "~" ,"`",gate.i[["channels"]][1],"`"))
    
    channel.ind<- match(gate.i[["channels"]],   as.character(lut$shortName))
    channel.char <- gate.i[["channels"]]
    
    if (any(lut[channel.ind, "scaleType"] == 4, na.rm = T)) { #transforms arcsinh channels approraitely
      ind <- which(lut[channel.ind, "scaleType"] == 4)
      flowCore::exprs(flow_frame)[,channel.char[ind]] <- asinh(flowCore::exprs(flow_frame)[,channel.char[ind]]/lut[channel.ind[ind], "cofactor"])
    }
    
    if(gate.i[["type"]] == "RectangleGate") {
      gate.i <- flowCore::rectangleGate(gate.i[["coords"]])
      flow_frame <- gatein(flow_frame, gate.i)
    } else if (gate.i[["type"]] == "PolygonGate") {
      gate.i <- flowCore::polygonGate(gate.i[["coords"]])
      flow_frame <- gatein(flow_frame, gate.i)
      
    } else if (gate.i[["type"]] == "EllipseGate") {
      colnames(gate.i[["cov_matrix"]]) <- gate.i[["channels"]]
      gate.i<- flowCore::ellipsoidGate(gate.i[["cov_matrix"]], mean = as.numeric(gate.i[["coords"]]))
      flow_frame<- gatein(flow_frame, gate.i)
    }  else if (gate.i[["type"]] == "RangeGate") {
      flow_frame <- flow_frame[
        flow_frame@exprs[,gate.i[["channels"]][1]] > gate.i[["coords"]][[1]] & 
          flow_frame@exprs[,gate.i[["channels"]][1]] < gate.i[["coords"]][[2]],
        ]
    } else if (gate.i["type"] == "QuadrantGate") {
      flow_frame<- flow_frame[
        flow_frame@exprs[,gate.i[["channels"]][1]] > gate.i[["coords"]][[1,1]] & 
          flow_frame@exprs[,gate.i[["channels"]][1]] < gate.i[["coords"]][[2,1]] &
          flow_frame@exprs[,gate.i[["channels"]][2]] > gate.i[["coords"]][[1,2]] &
          flow_frame@exprs[,gate.i[["channels"]][2]] < gate.i[["coords"]][[2,2]]
        ,]
    }
    
    if (any(lut[channel.ind, "scaleType"] == 4, na.rm = T)) { #backtransforms arcsinh channels back to linear scale
      ind <- which(lut[channel.ind, "scaleType"] == 4)
      flowCore::exprs(flow_frame)[,channel.char[ind]] <- sinh(flowCore::exprs(flow_frame)[,channel.char[ind]])*lut[channel.ind[ind], "cofactor"]
    }
  }
  return(flow_frame)
}
