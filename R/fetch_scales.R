#' A function for generating a scales and channel names look-up table from cytobank
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI:authentication]{autheticate}}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A data.frame for looking up scale and channel parameters
#' @importFrom CytobankAPI scales.list panels.list
fetch_scales <- function(cyto_session, exp_id) {

  scales <- CytobankAPI::scales.list(cyto_session,
                                     exp_id,
                                     output = "default")

  scales_df <- as.data.frame(lapply(scales, function(X) unname(unlist(X))))
  scales_df
  mypanels <- CytobankAPI::panels.list(cyto_session,
                                      exp_id,
                                      output = "default")

  panel.list <- vector('list', length = length(mypanels))
  names(panel.list) <- names(mypanels)
  panel.list
  for(i in seq(length(mypanels))){
    panel.i <- mypanels[[i]][['channels']]
    panel.i_df <- as.data.frame(lapply(panel.i, function(X) unname(unlist(X))))
    lut.i <- merge.data.frame(panel.i_df,
                            scales_df,
                            by.x= "normalizedShortNameId",
                            by.y ="normalizedShortNameId")

    panel.list[[i]]<- list(name =  names(mypanels)[i],
                           scales = lut.i,
                           fcsFileIDs = mypanels[[i]][['fcs_files']])
  }

  scales_df <- panel.list$`Panel 1`$scales %>%
    select(-c(longName, normalizedShortNameId, fcsFileChannelIndex,id, experimentId))
  return(scales_df)
}
