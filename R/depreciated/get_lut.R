#' A function for generating a scales and channel names look-up table from cytobank
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A data.frame for looking up scale and channel parameters
#' @export
#' @importFrom CytobankAPI scales.list panels.list

get_lut <- function(cyto_session, exp_id ) {

  scales <- CytobankAPI::scales.list(cyto_session,
                                     exp_id,
                                     output = "default")

  mypanel <- CytobankAPI::panels.list(cyto_session,
                                      exp_id,
                                      output = "default")[[1]][["channels"]]
  scales_df <- as.data.frame(lapply(scales, function(X) unname(unlist(X))))
  mypanel_df <- as.data.frame(lapply(mypanel, function(X) unname(unlist(X))))


  lut <- merge.data.frame(mypanel_df,
                          scales_df,
                          by.x= "normalizedShortNameId",
                          by.y ="normalizedShortNameId")
  extrarow <- data.frame(-1, "Null", "Null", -1, 0, 0, 0, 0, 0, 1)
  colnames(extrarow) <- colnames(lut)
  lut <- rbind(lut, extrarow)
  return(head(lut, -1))
}
