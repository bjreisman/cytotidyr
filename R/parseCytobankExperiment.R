#' Parses ACS files exported from Cytobank for use in flowWorkspace
#'
#' @param acspath A path to the exported ACS file
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
#' @return 10. A list of files unpacked from the ACS file, including FCS files
#' @export
#' @import CytobankAPI CytoML scales


parseCytobankExperiment <- function(acspath){
  if(length(grep("\\.acs$", basename(acspath))) == 0){
    stop("Not a valid ACS file")
  }
  ## Read in and convert parse the ACS file ------------------------------------
  message("Unpacking ACS file...")

  newname <- gsub('\\.acs$', '\\.zip', acspath)
  file.copy(acspath, newname)
  unzippath <- unzip(newname,
                     exdir =   dirname(newname))


  yamlpath<- unzippath[grep("\\.yaml$", unzippath)]
  experiment_yaml <- yaml::read_yaml(yamlpath)

  ## Extract the FCS file information ------------------------------------------
  fcs.file.i <- experiment_yaml$fcsFiles[[9]]

  fcs.file.tibble <-
    lapply(experiment_yaml$fcsFiles, function(fcs.file.i) {
      if (length(grep("panel", names(fcs.file.i))) == 0) {
        panel.i <- NA
      } else{
        panel.i <- fcs.file.i$panel$name
      }

      tibble(
        filename = fcs.file.i$filename,
        md5sum = fcs.file.i$md5sum,
        originalId = fcs.file.i$originalId,
        panel = panel.i,
        sampleName = fcs.file.i$sampleName
      )
    }) %>% bind_rows()

  message(paste(nrow(fcs.file.tibble), ' FCS files found'))

  # Renaming FCS files based on their filename ---------------------------------
  orig_fcsnames <- basename(grep("fcs_files", unzippath, value = T))
  new_fcsnames <- fcs.file.tibble[fcs.file.tibble$originalId == orig_fcsnames,"filename"]$filename

  orig_paths <- grep("fcs_files", unzippath, value = T)
  new_paths <- file.path(dirname(orig_paths), new_fcsnames)

  file.rename(orig_paths,
              new_paths)

  unzippath <- list.files(dirname(newname), full.names = T, recursive = T)

  ## Parse the Sample Tags -----------------------------------------------------
  sampletags <- lapply(experiment_yaml$fcsFiles, function(file.i) {
    as_tibble(file.i$tags) %>%
      mutate(filename = file.i$filename) %>%
    select(filename, everything())}) %>%
    bind_rows()
  tags.found <- paste(colnames(sampletags %>% select(-filename)),
                      collapse = "\n ")
  message(paste('Sample tags found for:\n', tags.found))

  ## Convert scales to table format --------------------------------------------
  ## similar to that return by cytobankAPI::scales.list()
  scales <-
    bind_rows(lapply(experiment_yaml$panels[[1]][[2]], as_tibble)) %>%
    mutate(channelShortName = tolower(shortName)) %>%
    left_join(bind_rows(lapply(experiment_yaml$scales, as_tibble)),
              by = "channelShortName") %>%
    mutate(scaleType = if_else(scaleType == "Linear", "1", scaleType),
           scaleType = if_else(scaleType == "Log", "2", scaleType),
           scaleType = if_else(scaleType == "Arcsinh", "4", scaleType),
           scaleType = as.numeric(scaleType))

  ### Parse the scales into a Transformer list accepted by flowWorkspace--------
  transforms <- parse_scales(scales)
  transforms.i <- transforms[[1]]

  ## didn't end up using this function, here, but may be useful elsewhere
  invertTransformerList <- function(transforms) {
    lapply(transforms, function(transforms.i) {
      forward <- transforms.i$transform
      reverse <- transforms.i$inverse
      transforms.i$transform <- reverse
      transforms.i$inverse <- forwards
    })
  }
  ## Parse the panels ----------------------------------------------------------
  panels <- lapply(experiment_yaml$panels, function(panel){
    suppressWarnings(
      bind_rows(lapply(panel[[2]], as.data.frame))
    )
  })

  names(panels) <-   unlist(lapply(experiment_yaml$panels, function(panel){
    panel[[1]]
  }))


  message(paste0("Panels:\n", paste(paste0(" ", names(panels)),
                                    collapse = "\n")))

  ## Parse Gates ---------------------------------------------------------------
  message(paste0("Importing Gates using CytoML"))
  gatingML_path <- unzippath[grep(unzippath, pattern = "_gate_")]
  gates <- CytoML::read.gatingML.cytobank(gatingML_path)

  ## Parse Compensations -------------------------------------------------------
  comps <- lapply(experiment_yaml$compensations, function(comp.i){
    mat.i<- do.call(rbind, comp.i$matrix)
    colnames(mat.i)   <- comp.i$channelShortNames
    rownames(mat.i) <- comp.i$channelShortNames
    mat.i
  })
  names(comps) <- lapply(experiment_yaml$compensations, function(comp.i){
    comp.i$name
  })

  message(paste0("Compensations:\n",
                 paste(paste0(" ", names(comps)),
                                    collapse = "\n")))

  message("Done!")
  # Return the output-----------------------------------------------------------
  return(list("fcs_files" = fcs.file.tibble,
              "sampletags" = sampletags,
              "scales" = scales,
              "transforms" = transforms,
              "compensations" = comps,
              "gates" = gates,
              "panels" = panels,
              "experiment.id" =   experiment_yaml$id,
              "experiment.name" =   experiment_yaml$name,
              "experiment.filepaths" = unzippath
       ))
}

