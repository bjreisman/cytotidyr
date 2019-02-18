#' Converts the scales table from cytobank to a transformlist for flowcore
#' @param exp_info object from get_experimentinfo
#' @param inverse, if true, returns the back-transformation to the original scales
#' @return a transformList object, with one slot for each channel
#' @import flowWorkspace scales
#' @export

parse_scales <- function(scales_tbl, inverse = F){
  row_to_transform <- function(scales_row) {
    channel <- as.character(scales_row[["shortName"]])
    if(scales_row["scaleType"] == 4) {
      cofactor <- as.numeric(scales_row["cofactor"])
      trans <- asinh_trans(cofactor)
    } else if (scales_row["scaleType"] == 2){
      trans <- scales::log10_trans()
    } else{
      trans <- scales::identity_trans()
    }

    return(trans)
  }
  tl <- transformerList(as.character(scales_tbl$shortName),
                      apply(scales_tbl, 1, row_to_transform))
}
