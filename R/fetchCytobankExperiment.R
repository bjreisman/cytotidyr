#' Imports Cytobank Experiment via the Cytobank API
#'
#' @param cyto_session A cytobank session created by \code{\link[CytobankAPI:authentication]{authenticate}}
#' @param exp_id The experiment id of the experiment to be retrived
#' @return A list of experiment information including
#' @return 1. A tibble of FCS files contained in the experiment and their panels
#' @return 2. A tibble of sample tags from the experiment
#' @return 3. A table of scales exported directly from cytobank
#' @return 4. A transformerList version of the scales for use in flowWorkspace
#' @return 5. A list of compensation matricies
#' @return 6. A gating + population heirarchy imported from Cytobank by CytoML
#' @return 7. A list of panels contained in the experiment
#' @return 8. The experiment ID (character vector)
#' @return 9. The experiment name (character vector)
#' @export
#' @import CytobankAPI CytoML dplyr


fetchCytobankExperiment <- function(cyto_session, exp_id){

  experiment.header <- CytobankAPI::experiments.show(cyto_session, exp_id)

  ## Extract the FCS file information ------------------------------------------
  fcs.file.list <- CytobankAPI::fcs_files.list(cyto_session, exp_id)
  cyto_session <- authenticate("vanderbilt", auth_token = token)
  fcs.file.tibble <- as_tibble(apply(as.matrix(fcs.file.list), 2, unlist)) %>%
    rename(originalId = "id") %>%
    select(c("filename","md5sum", "originalId", "sampleName"))

  message(paste(nrow(fcs.file.tibble), ' FCS files found'))

  ## Parse the Sample Tags -----------------------------------------------------
  sample.tag.path <- sample_tags.download(cyto_session, exp_id)
  sampletags <- suppressMessages(read.delim(sample.tag.path))
  file.remove(sample.tag.path)
  sampletags[sampletags == "-"] <- NA
  sampletags <- sampletags[,!apply(is.na(sampletags), 2, all)]

  tags.found <- paste(paste0("\t",
                             colnames(sampletags)[!apply(sampletags,
                                                         2,
                                                         function(x)
                                                           all(is.na(x)))]),
                      collapse = "\n")

  message(paste('Sample tags found for:\n', tags.found))

  ## Convert scales to table format --------------------------------------------
  scales <- get_scales(cyto_session, exp_id)

  ### Parse the scales into a Transformer list accepted by flowWorkspace--------
  transforms <- parse_scales(scales)

  ## Parse the panels ----------------------------------------------------------

  panels_raw <- CytobankAPI::panels.list(cyto_session,
                                       exp_id,
                                       output = "default")

  panels <- vector('list', length = length(panels_raw))
  panelsByFCS <- vector('list', length = length(panels_raw))

  names(panels) <- names(panels_raw)

  for(i in seq(length(panels_raw))){
    panels[[i]] <- as.data.frame(panels_raw[[i]][['channels']])
  }

  for (i in seq(length(panels_raw))) {
    panelsByFCS[[i]] <- data.frame(fcs_files = as.character(panels_raw[[i]][['fcs_files']]),
                                   panel = names(panels_raw)[i])
  }

  panel_lut <- suppressWarnings(bind_rows(panelsByFCS))

  panels <- lapply(panels, function(mydf) {
    channels <- unlist(mydf$longName)
    names(channels) <- unlist(mydf$shortName)
    channels
  })
  #adding panel assignments to fcs.file.tibble
  fcs.file.tibble <- fcs.file.tibble %>%
    left_join(panel_lut, by = c("originalId" = 'fcs_files')) %>%
    select(c("filename", 'md5sum', 'originalId', 'panel', 'sampleName'))

  message(paste0("Panels:\n", paste(paste0(" ", names(panels)),
                                    collapse = "\n")))

  ## Parse Gates ---------------------------------------------------------------
  message(paste0("Importing Gates using CytoML"))
  gatingML_path <- CytobankAPI::gates.gatingML_download(cyto_session, exp_id)
  gates <- CytoML::read.gatingML.cytobank(gatingML_path)
  file.remove(gatingML_path)
  ## Parse Compensations -------------------------------------------------------
  comps <- CytobankAPI::compensations.list(cyto_session,
                                           exp_id,
                                           output = 'default',
                                           timeout = 30)
  comps <- lapply(comps, function(comp.i) comp.i$compensation_matrix)

  message(paste0("Compensations:\n",
                 paste(paste0(" ", names(comps)),
                                    collapse = "\n")))

  message("Done!")
  ## Return the cytobank environemnt -------------------------------------------
  return(list("fcs_files" = fcs.file.tibble,
              "sampletags" = sampletags,
              "scales" = scales,
              "transforms" = transforms,
              "compensations" = comps,
              "gates" = gates,
              "panels" = panels,
              "experiment.id" =   experiment.header$id,
              "experiment.name" =   experiment.header$experimentName
  ))
}

