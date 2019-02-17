experiment_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("User's Experiments"),
    DT::dataTableOutput(ns("experiment_table")),
    actionButton(ns("submit_exp"), label = 'Select Experiment')
  )}



experiment_list <- function(input, output, session, api_connection, x){

  exp_table <- reactive({
    #print(api_connection)
    connection_status <- api_connection()[["check_if_connected"]][['bool']]

    if(connection_status){
      progress <- shiny::Progress$new()
      progress$set(message = "Fetching experiments from cytobank",
                   detail = ,
                   value = 0.2)
      on.exit(progress$close())
      n <- 2
      updateProgress <- function(detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
      # withProgress(message = 'fetching experiments from cytobank',
      #              detail = 'This may take a while...',
      #              value = 0,
      #              expr = {
      exps <- tryCatch({
        updateProgress(detail = "This may take a while...")
        CytobankAPI::experiments.list(UserSession = api_connection()[["cyto_session"]],
                                      timeout = api_connection()[["timeout"]]
                                      )[, c('id', 'experimentName')]
      })
      updateProgress(detail = "Done!")

      return(as.data.frame(apply(exps, 2, unlist))) #cytobank API now returns a list that needs to be unlisted... :\
      # })
    }
    else{
      return(data.frame())
    }
  })


  #table containing a list of user experiments
  output$experiment_table <- DT::renderDataTable({
    exp_table()
  }, selection = list(mode = 'single',
                      selected = c(1),
                      target = 'row'))


  experiment_info <- eventReactive(input$submit_exp, {
    print("button was pushed)")
    #print('line126')
    exp_row <- input$experiment_table_rows_selected
    #fcs_rows <- input$fcs_table_rows_selected

    exp_id <- unlist(exp_table()[exp_row,1])

    progress <- shiny::Progress$new()
    progress$set(message = "Gathering Experiment info...", value = 0.2)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    exp_info<-cytotidyr::get_experimentinfo(api_connection()[["cyto_session"]], exp_id = exp_id)

    return(exp_info)
  }
  )

  output$exper_info <- reactive({
    verbatimTextOutput(str(experiment_info()))
  })


  exp_info <- reactive({
    return(list('exp_info' = experiment_info()))
  })

  return(exp_info)
}
