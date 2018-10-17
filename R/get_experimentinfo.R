#' A simple wrapper for the population.list functions in cytobankAPI
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI]}{autheticate}
#' @param exp_id The experiment ID, found in the URL of the experiment
#' @return A list of experiment information including
#' @return 1. A tibble of FCS files contained in the experiment
#' @return 2. A tibble of sample tags from the experiment
#' @return 3. A list of scales + panels with the corresponding including channel names, limits, and transformations
#' @return 4. A list of compensation matricies
#' @return 5. A list of populations and their corresponding gates
#' @return 6. A integer corresponding to the experiment ID
#' @export
#' @importFrom CytobankAPI fcs file look-up table

get_experimentinfo <- function(cyto_session, exp_id){
  fcs.file.tibble <- get_fcsfilelut(cyto_session, exp_id)
  message(paste(length(fcs.file.tibble), ' FCS files found'))

  scales <- get_scales(cyto_session, exp_id)
  message(paste0("Panels:\n", paste(paste0("\t", names(scales)),
                                    collapse = "\n")))
  
  gates.raw <- get_gates(cyto_session, exp_id)
  gates.defined <- define_gates_rev(gates.raw, scales$`Panel 1`$scales)
  message(paste0(length(gates.defined), ' gates found\n'))
  
  sample.tag.path <- sample_tags.download(cyto_session, exp_id)
  sampletags <- suppressMessages(read_tsv(sample.tag.path))
  file.remove(sample.tag.path)
  sampletags[sampletags == "-"] <- NA
  tags.found <- paste(paste0("\t",colnames(sampletags)[!apply(sampletags, 2, function(x)all(is.na(x)))]),
                      collapse = "\n")
  message(paste('Sample tags found for:\n', tags.found))
  
  comps <- get_compensations(cyto_session, exp_id)
  
  message(paste0("Compensations:\n",
                 paste(paste0("\t", names(comps)),
                                    collapse = "\n")))
  
  populations <- get_populations(cyto_session, exp_id)
  message(paste0("Populations:\n",
                 paste(paste0("\t",   unlist(populations$name)),
                       collapse = "\n")))
  return(list("fcs_files" = fcs.file.tibble,
              "sampletags" = sampletags,
              "scales" = scales,
              "compensations" = comps, 
              "gates" = gates.defined, 
              "populations" = populations, 
              "experiment.id" = exp_id
       ))
}
