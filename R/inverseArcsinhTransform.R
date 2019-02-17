## Inverse of Asinh transformation constructor
#'
#' Creates the inverse of the asinh transformation, for backtransformations
#' @param transformationId character string to identify the transformation
#' @param a positive double that corresponds to a shift about 0.
#' @param b positive double that corresponds to a scale factor.
#' @param c positive double
#' @return Returns an object of class \code{transform}.
#' @family Transform functions
#' @seealso \code{\link{transform-class}}, \code{\link{transform}},
#' \code{asinh}
#' @keywords methods
inverseArcsinhTransform <- function(transformationId="defaultInverseArcsinhTransform",
                             a=1, b=1, c=0)
{
  t <- new("transform", .Data=function(x) (sinh(x-c)-a)/b)
  t@transformationId <- transformationId
  t
}
