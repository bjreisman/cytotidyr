importbyfcsfile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Select FCS Files"),
    DT::dataTableOutput(ns("fcs_table")),
    verbatimTextOutput(ns("test_fcs")),
    actionButton(ns("submit_download"), label = 'Download FCS files'),
    verbatimTextOutput(ns("filepaths_output"))
    )}



importbyfcsfile <- function(input, output, session, api_connection, exp_info_input, x){

  # exp_info_loaded <- readRDS("exp_info.rds")
  exp_info <- reactive(
    exp_info_input()[['exp_info']]
  )


  fcs_table <- reactive({
    fcs_table <- exp_info()[["fcs_files"]]
    #print(str(fcs_table))
    fcs_table[,c('id', 'filename')]
  })

  output$fcs_table <- DT::renderDataTable({
    fcs_table()
  }, selection = list(mode = 'multiple',
                      selected = c(1),
                      target = 'row'))

  output$test_fcs <- renderText({
    rows <- input$fcs_table_rows_selected
    fcsIDs <- unlist(fcs_table()[rows,1])
    print(fcsIDs)
    fcsIDs
  })

  #downloads fcs files
  filepaths <- eventReactive(input$submit_download, {
     fcs_rows <- input$fcs_table_rows_selected

     exp_ind <- exp_info()[['experiment.id']]
     fcs_ind <- unlist(fcs_table()[fcs_rows,1])

     n <- 3
     #Create a Progress object
     progress <- shiny::Progress$new()
     progress$set(message = "Downloading: ", value = 0)
     # Close the progress when this reactive exits (even if there's an error)
     on.exit(progress$close())

     updateProgress <- function(detail = NULL) {
       progress$inc(amount = 1/n, detail = detail)
     }

     dir.create('externaldata')
     zip.path<- fcs_files.download_zip(api_connection()[["cyto_session"]],
                            exp_info()[["experiment.id"]],
                            fcs_ind,
                            directory = 'externaldata')

     unzip.path <- unzip(zip.path, exdir = 'externaldata')
     return(unzip.path)
   })


  output$filepaths_output <- renderPrint(filepaths())
  return(filepaths())
}

