#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI:authentication]{autheticate}}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gates
#' @import CytobankAPI

get_fcsfilelut <- function(cyto_session, exp_id){
  #print(exp_id)
  fcs.file.list <- fcs_files.list(cyto_session, exp_id)
  fcs.file.tibble <- as_tibble(apply(as.matrix(fcs.file.list), 2, unlist)) %>%
    rename(originalId = "id") %>%
    select(c("filename","md5sum", "originalId", "sampleName"))
  #fcs.file.tibble
  return(return(fcs.file.tibble))
}
