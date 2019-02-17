## Inverse Logarithm transformation constructor
#' Create the definition of the inverse of a log transformation function (base specified by
#' user) to be applied on a data set
#' @usage logTransform(transformationId="defaultLogTransform", logbase=10, r=1, d=1)
#' @param transformationId character string to identify the transformation
#' @param logbase positive double that corresponds to the base of the
#' logarithm.
#' @param r positive double that corresponds to a scale factor.
#' @param d positive double that corresponds to a scale factor
#' @return Returns an object of class \code{transform}.
#' @family Transform functions
#' @seealso \code{\link{transform-class}}, \code{\link{transform}}
#' @keywords methods
#' @export
inverseLogTransform <- function(transformationId="inverseLogTransform",
                         logbase=10, r=1, d=1)
{
  if(!is.double(r) || r <= 0)
    stop("r must be numeric and positive")
  if(!is.double(d) || d <=0)
    stop("d must be numeric")
  if(!is.double(r) || r <=0)
    stop("r must be numeric and positive")
  if(!is.double(logbase) || logbase <= 1)
    stop("logabse must be a pnumeric greater than 1")
  t <- new("transform", .Data=function(x) x <- logbase^(x*d/r))
  t@transformationId <- transformationId
  t
}
