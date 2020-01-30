#' A hyperbolic arcsinh transformation for use within ggplot2.
#'
#' @param cofactor a cofactor to be used for the transformation
#' @return A transformation of the axis by the cofactor, such that
#' y = asinh(x/cofactor)
#' @seealso \code{\link[scales]{log_trans}} in the scales
#' package which implements a similar transformation
#' @importFrom scales trans_new
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds, aes(x = price)) +
#' geom_histogram() +
#' scale_x_continuous(trans = asinh_trans(5))
asinh_trans = function(cofactor) {

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
    #print(linear.ticks)
    log.ticks.major <- 10^seq(linear.ticks.max.10+2,6)
    log.ticks.minor.0 <- rep(10^seq(linear.ticks.max.10+2, 10, 1), each = 9)

    log.ticks.minor <- rep(1:9,length(log.ticks.minor.0)/9)*log.ticks.minor.0
    breaks <- c(0, log.ticks.major, -log.ticks.major, cofactor, -cofactor)
    labels <- c(breaks,
                rep("", length(linear.ticks)*2),
                rep("", length(log.ticks.minor)*2))
    breaks <- c(breaks, linear.ticks, -linear.ticks, log.ticks.minor, -log.ticks.minor)
    names(breaks) <- labels
    return(breaks)
  }
  # format = function(x,...){
  #   linear.ticks.max.10 <- floor(log10(cofactor))-1
  #   linear.ticks.max <- cofactor/(10^linear.ticks.max)
  #   linear.ticks <- c(seq(0, linear.ticks.max, 2))*10^linear.ticks.max.10
  #   log.ticks.major <- 10^seq(linear.ticks.max.10+1,6)
  #   log.ticks.minor <- rep(10^seq(linear.ticks.max.10+1, 5, 1), each = 8)
  #   labels <- c(0, log.ticks.major, -log.ticks.major)
  #   labels <- c(labels,
  #               rep("", length(linear.ticks)*2),
  #               rep("", length(log.ticks.minor)*2))
  #   print(labels)
  #   return(labels)
  # }

  scales::trans_new("asinh", transform, inverse, breaks)
}

scales::pseudo_log_trans


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
