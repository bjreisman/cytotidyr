#' S3 Method for the as.data.frame function to convert a flowset into a dataframe
#' @param x a flowSet object
#' @param add_filename if TRUE adds the filename of the original FCS file as a column
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @return A data.frame of all the cells in the flowSet
#' @export
#' @import flowCore

as.data.frame.flowSet <- function(x, ..., add_filename = TRUE, use_longnames = FALSE, add_pData = TRUE){
  x.list <- as.list(x@frames)
  myflowset.list <- lapply(x.list, function(ff) {
    filename <- ff@description$FILENAME
    ff.df <- as.data.frame(exprs(ff))
    if(use_longnames == TRUE){
      colnames(ff.df) <- ff@parameters@data$desc
   #   print(colnames(ff.df))
    }
    if (add_filename == TRUE) {
<<<<<<< HEAD
<<<<<<< HEAD
      ff.df[, "FCS Filename"] <- filename
=======
=======
>>>>>>> 44fcb803f49ea4e40f952c6eb9c5aa71cc7c83e2
      ff.df[, "FCS Filename"] <- basename(filename)
    }

    if (add_pData == TRUE) {
      if(ncol(pData(x)) > 1){
        tags.x <- pData(x)[basename(filename),]
        ff.df <- suppressWarnings(cbind(ff.df, tags.x[-1]))
      } else {
        warning("No pData found")
      }
<<<<<<< HEAD
>>>>>>> 4ba3ab6... removed cache
=======
>>>>>>> 44fcb803f49ea4e40f952c6eb9c5aa71cc7c83e2
    }
    return(ff.df)
  }
  )
  do.call(rbind, myflowset.list)
}
