#' Inverts a TransformerList object to return the 'back transformation'
#' @param transforms a transformList object
#' @return a transformList object
#' @export

invertTransformerList <- function(transforms) {
  transformsInverted<- lapply(transforms, function(transforms.i) {
    forward <- transforms.i$transform
    reverse <- transforms.i$inverse
    transforms.i$transform <- reverse
    transforms.i$inverse <- forward
    transforms.i$name <- paste0("inverse.", transforms.i$name)
    return(transforms.i)
  })
  return(transformsInverted)
}
