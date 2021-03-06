#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param exp_info cytobank experiment from fetchCytobankExperiment
#' @return A list of populations and how they are defined by gates
#' @export
#' @import CytobankAPI
apply_scales <- function(input, exp_info, inverse = FALSE){

  if(is_tibble(input)){
    input.tidy <- input
  }else if(class(input) == "flowFrame"){
    input.tidy <- as_tibble(exprs(input))
  } else {
    input.tidy <- as_tibble(input)
  }

  col.order <- colnames(input.tidy)

  transform.lut <-
    exp_info[['scales']] %>%
    dplyr::select(shortName, cofactor, scaleType) %>%
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
