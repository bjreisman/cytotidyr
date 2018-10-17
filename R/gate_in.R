#' A simple wrapper for the gating functions in flowCore
#'
#' @param data A flowframe containing the data to be gated
#' @param gate A flowCore gate object, as defined manually or by
#'  \code{\link{define_gates}}
#' @return A flow frame containing the data which lie within the selected gate
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @export
#' @importFrom flowCore Subset filter

gatein <- function(data, gate) { #this function subsets the data within the specified gate
  flowCore::Subset(data, flowCore::filter(data, gate))
}