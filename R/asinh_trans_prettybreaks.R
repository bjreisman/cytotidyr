#' A hyperbolic arcsinh transformation for use within ggplot2, that also makes nice breaks.
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

asinh_trans_prettybreaks = function(cofactor) {

  ### Making pretty tick marks
  transform =   function(x,...) {
    asinh(x/cofactor)
  }
  inverse = function(x,...) {
    sinh(x)*cofactor
  }
  breaks = function(x,...) {

    ###Making Pretty breaks
    ###Linear zone at the cofactor
    linear.ticks.max.10 <- floor(log10(cofactor))-1
    linear.ticks.max <- cofactor/(10^linear.ticks.max.10)
    linear.ticks <- c(seq(0, linear.ticks.max, length.out = 8))*10^linear.ticks.max.10
    #linear.ticks
    ##Log labels above the cofactor at the decades
    log.ticks.major <- 10^seq(linear.ticks.max.10+2,6)

    #Log ticks above the cofactor
    log.ticks.minor.0 <- rep(10^seq(linear.ticks.max.10+2, 10, 1), each = 9)
    log.ticks.minor <- rep(1:9,length(log.ticks.minor.0)/9)*log.ticks.minor.0
    log.ticks.minor <- log.ticks.minor[log.ticks.minor > (cofactor)]

    breaks <- c(0, log.ticks.major, -log.ticks.major, cofactor, -cofactor)
    labels <- c(breaks,
                rep("", length(linear.ticks)*2),
                rep("", length(log.ticks.minor)*2))
    breaks <- c(breaks, linear.ticks, -linear.ticks, log.ticks.minor, -log.ticks.minor)
    names(breaks) <- labels
    return(breaks)
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
