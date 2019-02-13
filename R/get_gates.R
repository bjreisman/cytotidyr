#' A simple function for dowloading gates from Cytobank.
#'
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI:authentication]{autheticate}}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A kist
#' @seealso \code{\link[CytobankAPI:compensations]{compensations.list}}
#' @import CytobankAPI


get_gates <- function(cyto_session, exp_id){
  return(CytobankAPI::gates.list(cyto_session, exp_id, output = "default"))
}
