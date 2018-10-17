#' A simple function for dowloading compensation matricies from Cytobank
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A compensation matrix (if one exists).
#' @seealso \code{\link[CytobankAPI]{compensations.list}}
#' @export
#' @import CytobankAPI

get_compensations <- function(cyto_session, exp_id) {
  return(CytobankAPI::compensations.list(cyto_session, exp_id, output = 'default', timeout = 30))
}