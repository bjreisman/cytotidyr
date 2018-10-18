#' A function that takes gates and scales returned by cytobank and generates flowcore compabitible gates
#'
#' @param gates returned by `get_gates`
#' @param lut, retruned by `get_expinfo`
#' @return A flow frame containing the data which lie within the selected gate
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}

# gates <- CytobankAPI::gates.list(cyto_session, experiment.id)
# gates
define_gates <- function(gates, lut) {

  #mygates <- vector("list", length =  length(unlist(gates$id)))
  #names(mygates) <- unlist(gates$id)
  mygates <- vector("list")

  for(i in 1:length(unlist(gates$id))) {
    ind <- gates$gateId[[i]]
    mygates[[ind]] <- vector("list")
    mygates[[ind]][["name"]] <- gates$name[[i]]
    mygates[[ind]][["gateID"]] <- gates$gateId[[i]]
    mygates[[ind]][["channels"]] <- c(gates$xNormalizedShortNameId[[i]], gates$yNormalizedShortNameId[[i]])
    mygates[[ind]][["channels"]] <- as.character(lut[match(mygates[[ind]][["channels"]],
                                                         lut$normalizedShortNameId),"shortName"])
    mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
    mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
    mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
    mygates[[ind]][["type"]] <- gates$type[[i]]
    names(mygates)[ind] <- mygates[[ind]][["name"]]

    if(mygates[[ind]][["type"]] == "PolygonGate") {
       mygates[[ind]][["coords"]] <- do.call(rbind,
                                          lapply(
                                            gates$definition[[i]][[1]][["polygon"]][["vertices"]],
                                            as.numeric))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
    } else if(mygates[[ind]][["type"]] == "RectangleGate") {
      mygates[[ind]][["coords"]] <- matrix(
        unlist(gates[["definition"]][[i]][[1]][["rectangle"]]), ncol = 2, byrow = TRUE)
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]

    } else if(mygates[[ind]][["type"]] == "EllipseGate") {
      #procedure for computing ellipsegate parameters from : https://support.bioconductor.org/p/35360/
      mydef <- (gates[["definition"]][[i]])[[1]]
      angle <- mydef$ellipse$angle
      major <- mydef$ellipse$major/2
      minor <- mydef$ellipse$minor/2

      m1 <- cos(angle)^2/(major^2) + (sin(angle)^2)/(minor^2)
      m2 <- sin(angle)*cos(angle)*(((1/major^2)) - (1/(minor^2)))
      m3 <- m2
      m4 <- sin(angle)^2/(major^2) + (cos(angle)^2)/(minor^2)
      mygates[[ind]][["cov_matrix"]] <- solve(matrix(c(m1, m2, m3, m4), nrow = 2))
      mygates[[ind]][["coords"]] <- matrix(unlist(mydef$ellipse$center), ncol = 2)
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]

    } else if(mygates[[ind]][["type"]]  == "SplitGate") {
      mydef <- gates$definition[[i]][[1]]
      channels.tmp <- mygates[[ind]][["channels"]]

      #left
      ind <- mydef[["split"]][["L"]]
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]]<- data.frame(-Inf, mydef[["split"]][["x"]])
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "RangeGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "low", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]


      #right
      ind <- mydef[["split"]][["R"]]
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]]<- data.frame(mydef[["split"]][["x"]], Inf)
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "RangeGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "high", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]

    } else if(mygates[[ind]][["type"]]  == "RangeGate") {
      mydef <- gates$definition[[i]][[1]]
      mygates[[ind]][["coords"]]<- data.frame(mydef$range$x1, mydef$range$x2)
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]

    } else if(mygates[[ind]][["type"]]  == "QuadrantGate") {
      mydef <- gates$definition[[i]][[1]]

      channels.tmp <- mygates[[ind]][["channels"]]
      # print('line 80')
      # print(str(mygates))
      # print(str(mygates[[ind]]))
      #UR
      ind <- mydef$quadrant$UR
      mygates[[ind]] <- list() #this overrides the originally created quandrant gate
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(mydef$quadrant$x, Inf, mydef$quadrant$y, Inf), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "UR", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]

      #UL
      ind <- mydef$quadrant$UL
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(-Inf, mydef$quadrant$x, mydef$quadrant$y, Inf), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "UL", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]


      #LL
      ind <- mydef$quadrant$LL
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(-Inf, mydef$quadrant$x, -Inf, mydef$quadrant$y), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "LL", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]


      #LR
      ind <- mydef$quadrant$LR
      mygates[[ind]] <- list()
      mygates[[ind]][["channels"]] <- channels.tmp
      mygates[[ind]][["coords"]] <- data.frame(matrix(c(mydef$quadrant$x, Inf, -Inf, mydef$quadrant$y), nrow = 2))
      colnames(mygates[[ind]][["coords"]]) <- mygates[[ind]][["channels"]]
      mygates[[ind]][["type"]] <- "QuadrantGate"
      mygates[[ind]][["gateID"]] <- ind
      mygates[[ind]][["name"]] <- paste0(gates$name[[i]], " (", "LR", ")")

      mygates[[ind]][["tailored"]] <-     unlist(gates$tailored[i])
      mygates[[ind]][["tailoredPerPopulation"]] <- unlist(gates$tailoredPerPopulation[i])
      mygates[[ind]][["fcsFileID"]] <- unlist(gates$fcsFileId[[i]])
      mygates[[ind]][["type"]] <- gates$type[[i]]
      names(mygates)[ind] <- mygates[[ind]][["name"]]


    } else  {
      warning(paste(mygates[[ind]][["type"]], "are not currently supported!"))
      break

    }
  }
  return(mygates)
}
