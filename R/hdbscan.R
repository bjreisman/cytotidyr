#' Conveinience function to perform HDBSCAN via reticulate.
#'
#' Performs HDBSCAN clustering on a n-dimensional matrix.
#' The algorithm is explained in detail in http://joss.theoj.org/papers/10.21105/joss.00205 and needs the python package \code{hdbscan} installed.
#'
#' @param embedding A matrix giving the embedding to be used for clustering.
#' @param min_samples Measure of how conservative the clustering should to be. The larger the value of \code{min_samples}, the more conservative the clustering and more points will be declared as noise, and clusters will be restricted to progressively more dense areas.
#' @param min_cluster_size The smallest size grouping that is considered a cluster.
#' @param outlier Determines how outliers are encoded in the resulting clustering.
#' @param algorithm Which algorithim to use: 'best', 'generic', 'prims_kdtree', 'prims_balltree', 'boruvka_kdtree', 'boruvka_balltree'.
#'
#' @param seed A numeric seed to initialize the random number generator.
#'
#' @return A factor with the assigned cluster.
#'
#' @export
hdbscan_ <- function(embedding,
                     min_samples = 7L,
                     min_cluster_size = 9L,
                     outlier = 0,
                     algorithm = 'best',
                     seed = NULL) {
  if(!is.matrix(embedding)) {
    embedding <- as.matrix(embedding)
  }

  if (!is.null(seed)) set.seed(seed)

  h <- reticulate::import("hdbscan")
  cl <- h$HDBSCAN(min_samples = min_samples,
                  min_cluster_size = min_cluster_size,
                  algorithm= algorithm)

  labels <- cl$fit_predict(embedding) + 1
  labels[labels == 0] <- outlier
  return(factor(labels))
}
