#' Plots points while computing a kernel density estimate

#' @export
#' @param n number of grid points in each direction
#' @param h Bandwidth (vector of length two). If `NULL`, estimated
#'   using [MASS::bandwidth.nrd()].
#' @import ggplot2
#' @section Computed variables:
#' \describe{
#'   \item{density}{the density estimate}
#'   \item{ndensity}{density estimate, scaled to maximum of 1}
#' }
stat_density_point_2d <- function(mapping = NULL, data = NULL,
                            geom = "point", position = "identity",
                            ...,
                            contour = FALSE,
                            n = 100,
                            h = NULL,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDensityPoint2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      contour = contour,
      n = n,
      h = h,
      ...
    )
  )
}

#' @export
#' @usage NULL
stat_densitypoint2d <- stat_density_point_2d
<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> 30a62ca... denisty points dependenity on ggproto fixed
=======
>>>>>>> 44fcb803f49ea4e40f952c6eb9c5aa71cc7c83e2
#' @import ggplot2
#' @format NULL
#' @usage NULL
#' @export
StatDensityPoint2d <- ggplot2::ggproto("StatDensityPoint2d", Stat,
  default_aes = aes(colour = stat(density), size = 0.5),

  required_aes = c("x", "y"),

  compute_group = function(data, scales, na.rm = FALSE, h = NULL,
                           contour = FALSE, n = 100, bins = NULL,
                           binwidth = NULL) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
    }

    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )

    ix <- findInterval(data$x, dens$x)
    iy <- findInterval(data$y, dens$y)
    ii <- cbind(ix, iy)

    df <- data.frame(x = data$x, y = data$y, z = dens$z[ii])
    df$group <- data$group[1]

    if (contour) {
        StatContour$compute_panel(df, scales, bins, binwidth)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$ndensity <- df$density / max(df$density, na.rm = TRUE)
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)
