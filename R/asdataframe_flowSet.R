#' S3 Method for the as.data.frame function to convert a flowset into a dataframe
#' @param x a flowSet object
#' @param add_filename if TRUE adds the filename of the original FCS file as a column
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @return A data.frame of all the cells in the flowSet
#' @export
#' @import flowCore

as.data.frame.flowSet <- function(x, ..., add_filename = TRUE, use_longnames = FALSE){
  x.list <- as.list(x@frames)
  myflowset.list <- lapply(x.list, function(ff) {
    filename <- ff@description$FILENAME
    ff.df <- as.data.frame(exprs(ff))
    if(use_longnames == TRUE){
      colnames(ff.df) <- ff@parameters@data$desc
   #   print(colnames(ff.df))
    }
    if (add_filename == TRUE) {
      ff.df[, "FCS Filename"] <- filename
    }
    return(ff.df)
  }
  )
  do.call(rbind, myflowset.list)
}
