#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gates
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @importFrom CytobankAPI populations.list

get_populations <- function(cyto_session, exp_id){
  #print(exp_id)
  return(CytobankAPI::populations.list(cyto_session, exp_id,  output = "default"))
}
