#' S3 Method for the as.data.frame function to convert a flowset into a data.frame
#' @param x a flowSet or ncdfFlowSet object
#' @param add_filename if TRUE adds the filename of the original FCS file as a column
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @param add_pdata if TRUE, adds pData as additional columns to the output
#' @param verbose if TRUE, will display warnings, otherwise will hide warnings
#' @return A tibble of all the cells in the flowSet
#' @export
#' @rawNamespace  import(ncdfFlow, except = filter)
#' @rawNamespace import(flowCore, except = c(filter, view))

as.data.frame.flowSet <- function(x, ..., add_filename = TRUE, use_longnames = FALSE, add_pData = TRUE, verbose = TRUE){

  if(class(x) == "ncdfFlowSet") {
    x <- ncdfFlow::as.flowSet(x)
  }
  x.list <- as.list(x@frames)
  myflowset.list <- lapply(x.list, function(ff) {
    filename <- ff@description$FILENAME
    ff.df <- as.data.frame(exprs(ff))
    if(use_longnames == TRUE){
      colnames(ff.df) <- ff@parameters@data$desc
   #   print(colnames(ff.df))
    }
    if (add_filename == TRUE) {
      ff.df[, "FCS Filename"] <- basename(filename)
    }

    if (add_pData == TRUE) {
      if(ncol(pData(x)) > 1){
        tags.x <- pData(x)[basename(filename),]
        ff.df <- suppressWarnings(cbind(ff.df, tags.x[-1]))
      } else {
        if(verbose == TRUE) {
          warning("No pData found")
        }
      }
    }
    return(ff.df)
  }
  )
  do.call(rbind, myflowset.list)
}


#' S3 Method for the as_tibble function to convert a flowset into a tibble via a data.frame
#' @param x a flowSet or ncdfFlowSet object
#' @param add_filename if TRUE adds the filename of the original FCS file as a column
#' @param use_longnames if TRUE, uses the longName from the FCS file as the column names
#' @param add_pdata if TRUE, adds pData as additional columns to the output
#' @param verbose if TRUE, will display warnings, otherwise will hide warnings
#' @return A tibble of all the cells in the flowSet
#' @export
#' @rawNamespace  import(ncdfFlow, except = filter)
#' @rawNamespace import(flowCore, except = c(filter, view))



as_tibble.flowSet <- function(x, ...) {
  x<- as.data.frame.flowSet(x, ...) %>%
    as_tibble()
}
