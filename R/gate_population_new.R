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

gate_population_rev2 <- function(flow_frame,
                                 population,
                                 exp_info, 
                                 verbose = TRUE) { #need to pass flow_frame (fcb flowCore file), gatelist (get_gates), population name (string), poplist (get_populations), lut
  # fcsFile_lut <- fcs.file.tibble
  # flow_frame <- ff.i
  # population <- populations$name[[2]]
  # poplist <- populations
  # gatelist <- gates.raw
  # gate_defs <- gates.defined
  #flow_frame <- myflowSet[[1]]
 # flow_frame <- myff.compd
  poplist <- exp_info$populations
  gate_defs <- exp_info$gates
  lut <- exp_info$scales[[1]]$scales
  fcsFile_lut <- exp_info$fcs_files
  
  #fcsFile_lut = NULL
  pop.gates <- unlist(poplist[[match(population, poplist$name), "definition"]][["gates"]]) # gets the sequence of gates (numeric ids) that defines a population

  gate.lut <- tibble(
    gateID =   names(gate_defs),
    gateName = as.character(lapply(gate_defs, `[[`, "name")),
    gateNum = as.numeric(lapply(gate_defs, `[[`, "gateID")),
    tailored = as.logical(lapply(gate_defs, `[[`, "tailored")),
    id = as.character(lapply(gate_defs, `[[`, "fcsFileID"))
  )
  #gate.lut
  gate.variant.count <- gate.lut %>%
    dplyr::filter(gateNum %in% pop.gates) %>%
    group_by(gateNum) %>%
    summarise(n = n())
  n.0 <- nrow(flow_frame.t)
  
  if(any(gate.variant.count[,"n"] != 1) & is.null(fcsFile_lut)) {
    stop("Attempted to apply tailored gate without providing an FCS file list.")
  } else{
    gate.lut <- gate.lut %>%
      left_join(fcsFile_lut %>% dplyr::select(id, filename), by = 'id')
  }
  
  #extract the filename from the flowframe
  ff_desc <- description(flow_frame)
  fcs.filename <- ff_desc$`$FIL`
  
  parent.name <- "ungated"
  cellyeild <- tibble(name = "ungated", 
                          n = nrow(flow_frame), 
                          percent_total = 100, 
                          percent_parent = 100, 
                          parent = "")
  
  col.order <- as.character(colnames(exprs(flow_frame)))
  transform.lut <- 
    lut %>%
    select(shortName, cofactor, scaleType) %>%
    gather(key, value, cofactor, scaleType) %>%
    spread(shortName, value) %>%
    column_to_rownames("key") %>%
    select(col.order)
  
  flow_frame.t <- flow_frame
  exprs(flow_frame.t) <- as_tibble(exprs(flow_frame)) %>%
    select(col.order) %>%
    map2_dfc(transform.lut, 
             function(x,y){
               if(y[2] ==2){
                 return(log10(x))
               } else if(y[2] ==4 ){
                 return(asinh(x/y[1]))
               } else {
                 return(x)
               }
             }) %>%
    as.matrix()
  
  for(j in pop.gates) { #for gate in pop_gates

    n.parent <- nrow(flow_frame.t)
    
    gate.variant.lut <- gate.lut %>%
      dplyr::filter(gateNum == j)
    #print(gate.variant.lut)
    #checks if there exists a tailored gate for the flowframe
    if(nrow(gate.variant.lut) == 1){
      #not a tailored gate, returns gate.i
      gate.i <- gate_defs[[unlist(gate.variant.lut[1,"gateID"])]]
      #print('non tailored gate')
    } else if(fcs.filename %in% gate.variant.lut$filename) { 
      #retruns tailored verison of the gate
      gateID.i <- gate.variant.lut %>%
        dplyr::filter(filename == fcs.filename)
      gate.i <- gate_defs[[gateID.i$gateID]]
     # print('tailored gate')
      
    } else { #uses the non-tailored version
      gateID.i <- gate.variant.lut %>%
        dplyr::filter(is.na(filename))
      gate.i <- gate_defs[[gateID.i$gateID]]
      #print('tailored gate, base gate')
    }
    gate.i.name <- gate.i$name
    axis <- as.formula(paste0("`",gate.i[["channels"]][2],"`", "~" ,"`",gate.i[["channels"]][1],"`"))

    channel.ind<- match(gate.i[["channels"]], as.character(lut$shortName))
    channel.char <- gate.i[["channels"]]

    # if (any(lut[channel.ind, "scaleType"] == 4, na.rm = T)) { #transforms arcsinh channels approraitely
    #   exprs(flow_frame)
    #   ind <- which(lut[channel.ind, "scaleType"] == 4)
    #   flowCore::exprs(flow_frame)[,channel.char[ind]] <- asinh(flowCore::exprs(flow_frame)[,channel.char[ind]]
    #                                                            /
    #                                                              lut[channel.ind[ind], "cofactor"])
    # 
    # 
    # }
    # 
    # 
    # 
    # plot.i <- 
    #   as_tibble(exprs(flow_frame.t)) %>%
    #   # mutate(`APC-H7-A` = asinh(`APC-H7-A`/500),
    #   #        `Alexa Fluor 700-A` = asinh(`Alexa Fluor 700-A`/500)) %>%
    #   ggplot(aes_string(x ="`APC-H7-A`",
    #                     y = "`Alexa Fluor 700-A`")
    #   ) + 
    #   geom_bin2d(bins = 300) +
    #   scale_fill_viridis_c(option = "A") + 
    #   labs(subtitle = paste0(nrow(exprs(flow_frame.t))/nrow(parent.tibble)*100), "%")
    # ###########3
    # ggsave(paste0("plot",j,".png"), plot.i)
    
    if(gate.i[["type"]] == "RectangleGate") {
      gate.rect <- as_tibble(gate.i[["coords"]])
      gate.i <- flowCore::rectangleGate(gate.i[["coords"]])
      # parent.tibble <- as_tibble(exprs(flow_frame))
      flow_frame.t <- gatein(flow_frame.t, gate.i)
      
      # plot.i <- 
      #   bind_rows(parent.tibble %>% mutate(class = 'parent'), 
      #             as_tibble(exprs(flow_frame)) %>% mutate(class = 'child')) %>%
      #   ggplot(aes_string(x = paste0("\`", colnames(gate.rect)[1],"\`"),
      #                     y = paste0("\`", colnames(gate.rect)[2],"\`")
      #   )) + 
      #   geom_bin2d(bins = 300) +
      #   scale_fill_viridis_c(option = "A") + 
      #   geom_rect(xmin = as.numeric(gate.rect[1,1]), 
      #             xmax = as.numeric(gate.rect[2,1]), 
      #             ymin = as.numeric(gate.rect[1,2]),
      #             ymax = as.numeric(gate.rect[2,2]), 
      #   data = gate.polygon, 
      #   inherit.aes = F,
      #   fill = NA, 
      #   col = "red", 
      #   size = 1) + 
      #   labs(subtitle = paste0(nrow(exprs(flow_frame))/nrow(parent.tibble)*100), "%") + 
      #   facet_grid(.~class)
      # 
      # 
      # 
      # ############
    } else if (gate.i[["type"]] == "PolygonGate") {
      
      ###########
      gate.polygon <- as_tibble(gate.i[["coords"]])
      # parent.tibble <- as_tibble(exprs(flow_frame))
      gate.i <- flowCore::polygonGate(gate.i[["coords"]])
     #  
      flow_frame.t <- gatein(flow_frame.t, gate.i)
     # 
     #  plot.i <- 
     #    bind_rows(parent.tibble %>% mutate(class = 'parent'), 
     #              as_tibble(exprs(flow_frame)) %>% mutate(class = 'child')) %>%
     #    ggplot(aes_string(x = paste0("\`", colnames(gate.polygon)[1],"\`"),
     #                      y = paste0("\`", colnames(gate.polygon)[2],"\`")
     #                      )) +
     #    geom_bin2d(bins = 300) +
     #    scale_fill_viridis_c(option = "A") +
     #    geom_polygon(aes_string(x = paste0("\`", colnames(gate.polygon)[1],"\`"),
     #                            y = paste0("\`", colnames(gate.polygon)[2],"\`")
     #    ),
     #    data = gate.polygon,
     #    fill = NA,
     #    col = "red",
     #    size = 1) +
     #    labs(subtitle = paste0(nrow(exprs(flow_frame))/nrow(parent.tibble)*100), "%") + 
     #    facet_grid(.~class)
     #  
     # 
     # 
     # # ggsave(paste0("plot",j,".png"), plot.i)
     #  
     #  ############

    } else if (gate.i[["type"]] == "EllipseGate") {
      colnames(gate.i[["cov_matrix"]]) <- gate.i[["channels"]]
      gate.i<- flowCore::ellipsoidGate(gate.i[["cov_matrix"]], mean = as.numeric(gate.i[["coords"]]))
      flow_frame.t<- gatein(flow_frame.t, gate.i)
    }  else if (gate.i[["type"]] == "RangeGate") {
      flow_frame.t <- flow_frame.t[
        flow_frame.t@exprs[,gate.i[["channels"]][1]] > gate.i[["coords"]][[1]] & 
          flow_frame.t@exprs[,gate.i[["channels"]][1]] < gate.i[["coords"]][[2]],
        ]
    } else if (gate.i["type"] == "QuadrantGate") {
      flow_frame.t<- flow_frame.t[
        flow_frame.t@exprs[,gate.i[["channels"]][1]] > gate.i[["coords"]][[1,1]] & 
          flow_frame.t@exprs[,gate.i[["channels"]][1]] < gate.i[["coords"]][[2,1]] &
          flow_frame.t@exprs[,gate.i[["channels"]][2]] > gate.i[["coords"]][[1,2]] &
          flow_frame.t@exprs[,gate.i[["channels"]][2]] < gate.i[["coords"]][[2,2]]
        ,]
    }
    
    
    
    # 
    # if (any(lut[channel.ind, "scaleType"] == 4, na.rm = T)) { #backtransforms arcsinh channels back to linear scale
    #   print(as_tibble(exprs(flow_frame)))
    #   ind <- which(lut[channel.ind, "scaleType"] == 4)
    #   print(channel.char[ind])
    #   flowCore::exprs(flow_frame)[,channel.char[ind]] <- sinh(flowCore::exprs(flow_frame)[,channel.char[ind]])*lut[channel.ind[ind], "cofactor"]
    # }
    n <- nrow(flow_frame.t)
    cellyeild<- bind_rows(cellyeild,
              tibble(name = gate.i.name,
               n = n,
               percent_total = round(n/n.0*100,3),
               percent_parent = round(n/n.parent*100,3), 
               parent = parent.name)
    )
    # message(paste(gate.i.name,"\t", n, "cells"))
    # message(paste0("\tPercent of total\t", round(n/n.0*100,2), "%"))
    # message(paste0("\tPercent of ", gate.i.name, "\t", round(n/n.parent*100,2), "%"))
    
    
    parent.name <- gate.i.name
  }
  if(verbose == TRUE){
    print(cellyeild)
  }
  flow_frame.bt <- flow_frame.t
  exprs(flow_frame.bt) <- as_tibble(exprs(flow_frame.t)) %>%
    select(col.order) %>%
    map2_dfc(transform.lut, 
             function(x,y){
               if(y[2] ==2){
                 return(10^(x))
               } else if(y[2] ==4 ){
                 return(sinh(x)*y[1])
               } else {
                 return(x)
               }
             }) %>%
    as.matrix()
  return(flow_frame.bt)
}
