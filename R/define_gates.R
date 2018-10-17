#' A function that takes gates and scales returned by cytobank and generates flowcore compabitible gates
#'
#' @param gates returned by `get_gates`
#' @param lut, retruned by `get_expinfo`
#' @return A flow frame containing the data which lie within the selected gate
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @export


define_gates_rev <- function(gates, lut) {
  #exp_info$exp_gates
  #gates <- exp_info$exp_gates
  #lut <- exp_info$exp_lut
  #gates$name
  #gates <- gates.raw
  #lut
  #unlist(gates$name)
  mygates <- vector("list", length =  length(unlist(gates$id)))
  names(mygates) <- unlist(gates$id)
  #i <- 1
  # gates$name[[4]]
  # i <- 4
  
  #mygates[[i]]
  for(i in 1:length(mygates)) {
    # #j <- gates$id[[i]]
    # print(i)
    # print(gates$xNormalizedShortNameId[[i]])
    # print(gates$yNormalizedShortNameId[[i]])
    # print(mygates[[i]][["channels"]])
    # str(mygates[[i]])
    mygates[[i]] <- vector("list")
    mygates[[i]][["name"]] <- gates.raw$name[[i]]
    mygates[[i]][["gateID"]] <- gates$gateId[[i]]
    mygates[[i]][["channels"]] <- c(gates$xNormalizedShortNameId[[i]], gates$yNormalizedShortNameId[[i]])
    mygates[[i]][["channels"]] <- as.character(lut[match(mygates[[i]][["channels"]],
                                                         lut$normalizedShortNameId),"shortName"])
    mygates[[i]][["tailored"]] <-     unlist(gates$tailored[i])
    mygates[[i]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
    mygates[[i]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
    mygates[[i]][["type"]] <- gates$type[[i]]
    
    if(mygates[[i]][["type"]] == "PolygonGate") {
       mygates[[i]][["coords"]] <- do.call(rbind,
                                          lapply(
                                            gates$definition[[i]][[1]][["polygon"]][["vertices"]],
                                            as.numeric))
      colnames(mygates[[i]][["coords"]]) <- mygates[[i]][["channels"]]
    } else if(mygates[[i]][["type"]] == "RectangleGate") {
      mygates[[i]][["coords"]] <- matrix(
        unlist(gates[["definition"]][[i]][[1]][["rectangle"]]), ncol = 2, byrow = TRUE)
      colnames(mygates[[i]][["coords"]]) <- mygates[[i]][["channels"]]
      
    } else if(mygates[[i]][["type"]] == "EllipseGate") {
      #procedure for computing ellipsegate parameters from : https://support.bioconductor.org/p/35360/
      mydef <- (gates[["definition"]][[i]])[[1]]
      angle <- mydef$ellipse$angle
      major <- mydef$ellipse$major/2
      minor <- mydef$ellipse$minor/2
      
      m1 <- cos(angle)^2/(major^2) + (sin(angle)^2)/(minor^2)
      m2 <- sin(angle)*cos(angle)*(((1/major^2)) - (1/(minor^2)))
      m3 <- m2
      m4 <- sin(angle)^2/(major^2) + (cos(angle)^2)/(minor^2)
      mygates[[i]][["cov_matrix"]] <- solve(matrix(c(m1, m2, m3, m4), nrow = 2))
      mygates[[i]][["coords"]] <- matrix(unlist(mydef$ellipse$center), ncol = 2)
      colnames(mygates[[i]][["coords"]]) <- mygates[[i]][["channels"]]
      
    } else if(mygates[[i]][["type"]]  == "SplitGate") {
      mydef <- gates$definition[[i]][[1]]
      
      #left
      ind <- mydef[["split"]][["L"]]
      names(mygates)[ind] <- paste0(names(mygates)[i],"_","low") #define gate name
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]]<- data.frame(-Inf, mydef[["split"]][["x"]])
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "RangeGate"
      
      
      #right
      ind <- mydef[["split"]][["R"]]
      names(mygates)[ind] <- paste0(names(mygates)[i],"_","high") #define gate name
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]]<- data.frame(mydef[["split"]][["x"]], Inf)
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "RangeGate"
      
    } else if(mygates[[i]][["type"]]  == "RangeGate") {
      mydef <- gates$definition[[i]][[1]]
      mygates[[i]][["coords"]]<- data.frame(mydef$range$x1, mydef$range$x2)
      colnames(mygates[[i]][["coords"]]) <- mygates[[i]][["channels"]]
      
    } else if(mygates[[i]][["type"]]  == "QuadrantGate") {
      mydef <- gates$definition[[i]][[1]]
      
      #UR
      ind <- mydef$quadrant$UR
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(mydef$quadrant$x, Inf, mydef$quadrant$y, Inf), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      
      #UL
      ind <- mydef$quadrant$UL
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(-Inf, mydef$quadrant$x, mydef$quadrant$y, Inf), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      
      #LL
      ind <- mydef$quadrant$LL
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(-Inf, mydef$quadrant$x, -Inf, mydef$quadrant$y), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      
      #LR
      ind <- mydef$quadrant$LR
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- mygates[[i]][["channels"]]
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(mydef$quadrant$x, Inf, -Inf, mydef$quadrant$y), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      
    } else  {
      warning(paste(mygates[[i]][["type"]], "are not currently supported!"))
      break #all gate types are supported at this point, future implementations will also include tailored gates. 
      
    }
    #print(str(mygates[[i]]))
    #mygates[[i]][["coords"]]
    #print(mygates[[i]][["coords"]])
    #colnames(mygates[[i]][["coords"]]) <- mygates[[i]][["channels"]]
  }
  return(mygates)
}
