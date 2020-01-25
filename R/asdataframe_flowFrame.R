#' S3 Method for the as.data.frame function to convert a flowFrame into a dataframe
#' @param x a flowFrame object
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @return A data.frame of all the cells in the flowFrame
#' @export
#' @import import(flowCore, except = filter)


as.data.frame.flowFrame <- function(x, ..., use_longnames = FALSE){

  df.tmp <- as.data.frame(exprs(x))
  if(use_longnames == TRUE){
    colnames(df.tmp) <- x@parameters@data$desc
  }
  df.tmp
}
