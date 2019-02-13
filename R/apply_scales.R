#' A simple wrapper for the population.list functions in cytobankAPI
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI:authentication]{CytobankAPI:authenticate}}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gate
#' @export
#' @import CytobankAPI

apply_scales <- function(input, exp_info, inverse = FALSE){
  #print(exp_info)
  apply_scales_internal <- function(input.tidy) {
    col.order <- colnames(input.tidy)
   # print("running 14")
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
    return(output.tidy)
  }


  if(is_tibble(input)){
    input.tidy <- input
    output <- apply_scales_internal(input.tidy)

  }else if(class(input) == "flowFrame"){
    input.tidy <- as_tibble(exprs(input))
    output <- input
    output.tidy <- apply_scales_internal(input.tidy)
    exprs(output)<- as.matrix(output.tidy)

  } else if(class(input) == "flowSet"){
    ff.list <-  as.list(input@frames)
    output <- input

    output.tidy <- lapply(ff.list, function(ff) as_tibble(exprs(ff)))
    output.tidy <- lapply(output.tidy, apply_scales_internal)
    #output.tidy$`NADH Only_Olgioat60Sec_001.fcs`$`PE-A`
    output.list <-
      mapply(function(input.ff, scaled.tb) {
        flowCore::exprs(input.ff) <- as.matrix(scaled.tb)
        return(input.ff)
      },
      input.ff = ff.list,
      scaled.tb = output.tidy)

    output <- flowSet(output.list)

  } else {
    input.tidy <- as_tibble(input)
    output <- apply_scales_internal(input.tidy)
  }
  return(output)
}
