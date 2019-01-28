#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gates
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @import CytobankAPI

get_fcsfilelut <- function(cyto_session, exp_id){
  #print(exp_id)
  fcs.file.list <- fcs_files.list(cyto_session, exp_id)
  fcs.file.tibble <- as_tibble(apply(as.matrix(fcs.file.list), 2, unlist))
  return(return(fcs.file.tibble))
}

