#' A hyperbolic arcsinh transformation for use within ggplot2.
#'
#' @param cofactor a cofactor to be used for the transformation
#' @return A transformation of the axis by the cofactor, such that 
#' y = asinh(x/cofactor)
#' @seealso \code{\link[scales]{pseudo_log_trans}} in the scales 
#' package which implements a similar transformation
#' @export
#' @importFrom scales trans_new
#' @examples
#' library(ggplot2)
#' 
#' ggplot(diamonds, aes(x = price)) + 
#' geom_histogram() + 
#' scale_x_continuous(trans = asinh_trans(5))

asinh_trans = function(cofactor) {
  transform =   function(x,...) {
    asinh(x/cofactor)
  }
  inverse = function(x,...) {
    sinh(x)*cofactor
  }
  breaks = function(x) {
    minor.ticks <- (rep(2:9, 6))*(rep(10^(0:5), each = 8))
    major.ticks <- 10^(2:6)
    breaks <-(c(0, major.ticks,- major.ticks))
    breaks[order(breaks)]
  }
  scales::trans_new("asinh", transform, inverse, breaks)
}



# asinh_trans = function(cofactor) {
#   transform =   function(x,...) {
#     asinh(x/cofactor)/log(10) + log(cofactor/2)/log(10)
#   }
#   inverse = function(x,...) {
#     sinh(x*log(10) - log(cofactor/2))*cofactor
#   }
#   breaks = function(x) {
#     minor.ticks <- (rep(2:9, 6))*(rep(10^(0:5), each = 8))
#     major.ticks <- 10^(2:6)
#     breaks <-(c(0, major.ticks,- major.ticks))
#     breaks[order(breaks)]
#   }
#   scales::trans_new("asinh", transform, inverse, breaks)
# }
