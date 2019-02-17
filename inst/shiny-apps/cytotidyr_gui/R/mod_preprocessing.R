preprocess_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Select Compensation"),
    h4("Select Population"),
    h4("Select Panel"),
    h4("Apply Scales?")
  )}



preprocess <- function(input, output, session, exp_info_input, fcs_path, x){

  exp_info_loaded <- readRDS("exp_info.rds")
  exp_info <- reactive(
    exp_info_loaded
  )
  output$fcsfile_table <- DT::renderDataTable({
    exp_info()[["fcs_files"]][,"filename"]
  }, selection = list(mode = 'multiple',
                      target = 'row'))
}

list.files("R")
