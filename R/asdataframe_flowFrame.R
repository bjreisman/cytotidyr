#' S3 Method for the as.data.frame function to convert a flowFrame into a dataframe
#' @param x a flowFrame object
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @return A data.frame of all the cells in the flowFrame
#' @export
#' @rawNamespace import(flowCore, except = c(filter, view))


as.data.frame.flowFrame <- function(x, ..., use_longnames = TRUE){
  ff.df <- as.data.frame(exprs(x))
  if(use_longnames == TRUE){
    longnames.tmp <- x@parameters@data$desc
    longnames.notna <- !is.na(longnames.tmp)
    colnames(ff.df)[longnames.notna] <- longnames.tmp[longnames.notna]
  }
  ff.df
}
