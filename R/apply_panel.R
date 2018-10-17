#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of populations and how they are defined by gates
#' @seealso \code{\link[flowCore]{Subset}}
#' @seealso \code{\link[flowCore]{filter}}
#' @export
#' @importFrom CytobankAPI fcs file look-up table

apply_panel <- function(input, exp_info, panel = NULL inverse = FALSE 
                        ){
  #panel <- "Panel 2"
  scales.i <- exp_info$scales
  if(length(scales.i) >1 & is.null(panel)){
    names(scales.i)
    warning(paste0("More than one panel found, please specify `panel =`\n"), 
            paste0(paste0("-", names(scales.i)), collapse = "\n"))
    return(NULL)
  } else if(!panel %in% names(scales.i)){
    warning(paste0(panel, " not found. Please elect from:\n", 
            paste0(paste0("-", names(scales.i)), collapse = "\n")))
    return(NULL)
  }


  if(is_tibble(input)){
    input.tidy <- input
  }else if(class(input) == "flowFrame"){
    input.tidy <- as_tibble(exprs(input))
  } else {
    input.tidy <- as_tibble(input)
  }

  longName <- as.character(scales.i[[panel]][["scales"]]$longName)
  shortName <- as.character(scales.i[[panel]][["scales"]]$shortName)
  names(shortName) <- longName
  
  if(is_tibble(input)){
    output.tidy<- input.tidy %>%
      rename(!!!shortName)
    output <- output.tidy
  }else if(class(input) == "flowFrame"){
    output <- input
    markernames.tmp <- names(shortName)
    names(markernames.tmp) <- shortName
    markernames(output) <- markernames.tmp
  } else{
    output <- output.tidy
  }
  
  return(output)
}
