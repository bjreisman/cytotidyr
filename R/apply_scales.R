#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gates
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @export
#' @import CytobankAPI

apply_scales <- function(input, exp_info, inverse = FALSE){
#
#   input <- mydata.list[[1]]
#   exp_info <- exp_info

  if(is_tibble(input)){
    input.tidy <- input
  }else if(class(input) == "flowFrame"){
    input.tidy <- as_tibble(exprs(input))
  } else {
    input.tidy <- as_tibble(input)
  }

  col.order <- colnames(input.tidy)

  transform.lut <-
    exp_info$scales[[1]]$scales %>%
    select(shortName, cofactor, scaleType) %>%
    gather(key, value, cofactor, scaleType) %>%
    spread(shortName, value) %>%
    column_to_rownames("key")

  transform.order <- colnames(transform.lut)
  output.tidy <- input.tidy

  if (inverse == FALSE){
    output.tidy <- input.tidy %>%
      select(transform.order) %>%
      map2_dfc(transform.lut,
               function(x,y){
                 if(y[2] ==2){
                   return(log10(x))
                 } else if(y[2] ==4 ){
                   return(asinh(x/y[1]))
                 } else {
                   return(x)
                 }
               })
  } else {
    output.tidy <- input.tidy %>%
      select(transform.order) %>%
      map2_dfc(transform.lut,
               function(x,y){
                 if(y[2] ==2){
                   return(10^(x))
                 } else if(y[2] ==4 ){
                   return(sinh(x)*y[1])
                 } else {
                   return(x)
                 }
               })
  }

  output.tidy <- output.tidy %>%
    bind_cols(input.tidy %>%
                select(-transform.order)) %>%
    select(col.order)

  if(is_tibble(input)){
    output <- output.tidy
  }else if(class(input) == "flowFrame"){
    output <- input
    exprs(output)<- output.tidy %>% as.matrix()
  } else{
    output <- output.tidy
  }

  return(output)
}
