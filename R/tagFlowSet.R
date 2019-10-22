#' Updates pData based on cytobank sampletags
#'
#' @param myflowset a flowSet created by flowCore
#' @param sampletags a table of sampletags, containing a "FCS.Filename" column (at minimum)
#' @return A flowSet with the pData slot updated based on the sample tags
#' @export
#' @import tibble dplyr flowWorkspace


tagFlowSet <- function(myflowset, sampletags) {
  origpData <- flowWorkspace::pData(myflowset)

  newpData <-
    sampletags[match(origpData$name, sampletags$FCS.Filename),] %>%
    mutate(name = as.character(FCS.Filename)) %>%
    select(name, everything()) %>%
    as.data.frame() %>%
    tibble::column_to_rownames("FCS.Filename")

  flowWorkspace::pData(myflowset) <- newpData
  return(myflowset)
}

