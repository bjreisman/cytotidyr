

api_mode_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4('Connect to Cytobank'),
    selectInput(ns("domain"),
                label = 'Domain',
                choices = c('vanderbilt',
                            'cellmass'),
                selected = 'vanderbilt'),
    passwordInput(ns("token"),
                  label = 'API Token',
                  placeholder = "Copy + paste from Cytobank"),
    actionButton(ns("submit_connection"),
                 label = "Submit"),
    helpText("API tokens can be generated in cytobank under:",
             br(),
             "'account settings' > 'API token' > 'Generate API token'"),
    numericInput(ns("cytobank_timeout"),
                 value = 360,
                 label = "API Timeout (seconds)"),
    textOutput(ns("connected")),
    hr(),
    h6("Created by Benjamin Reisman and David Earl, 2017"),
    h6("Updated: 5/1/2018")

  )
}

## server function

api_mode <- function(input, output, session, x){
  login_info <- reactive({
    #will only run authenticate if user name and token are entered
    validate(c(
      need(input$token, message = "stil waiting for token")))
    token <- input$token
    return(token)
  })

  #funky behavior now, i think when renderUI is called
  #input$submit_connection resets
  #cyto_session must be triggered again
  cyto_session <- eventReactive(input$submit_connection, {
    print("submitted")
    userlogin <- login_info()
    attempt <- CytobankAPI::authenticate(site=input$domain,
                                         auth_token = userlogin,
                                         timeout = input$cytobank_timeout)
    return(attempt)
  })

  #not sure how this works
  check_if_connected <- function(cyto_session) {
    connection_status <- tryCatch({
      exp.list <- CytobankAPI::experiments.list(cyto_session, timeout = 1)
    },
    error = function(cond) {
      if(grepl("Timeout", cond)) {
        return(list('bool' = TRUE,
                    'status' = "connected"))
      }
      else {
        return(list('bool' = FALSE,
                    'status' = cond))
      }
    },
    warning = function(cond) {
      return(list('bool' = FALSE,
                  'status' = cond))
    })
    return(connection_status)
  }

  check_if_connected_reactive <- reactive({
    check_if_connected(cyto_session())
  })

  output$connected <- renderText({
    connection_status <- check_if_connected_reactive()
    if(connection_status[['bool']]) {
      return(paste("connected to", cyto_session()@site))
    }
    else {
      return(connection_status[['status']][[1]])
    }
  })

  exp_table <- reactive({
    connection_status <- check_if_connected_reactive()[['bool']]
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
        CytobankAPI::experiments.list(cyto_session(), timeout = input$cytobank_timeout)[, c('id', 'experimentName')]
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
      #print('line126')
      exp_row <- input$experiment_table_rows_selected
      #fcs_rows <- input$fcs_table_rows_selected

      exp_id <- unlist(exp_table()[exp_row,1])

        progress <- shiny::Progress$new()
        progress$set(message = "Gathering Experiment info...", value = 0.2)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        n <- 7
        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1/n, detail = detail)
        }
        #exp_id <- exp_ind
        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Populations")
        }
        exp_pops <- get_populations(cyto_session(), exp_id)
        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Compesnations")
        }
        exp_comps <- get_compensations(cyto_session(), exp_id)

        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Gating Scheme")
        }
        exp_gates <- get_gates(cyto_session(), exp_id)
        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Scales")
        }
        exp_lut <- get_lut(cyto_session(), exp_id)

        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Files")
        }
        fcs_table <- CytobankAPI::fcs_files.list(cyto_session(), exp_id)
        exp_fcs <- fcs_table[,c('id', 'filename')]

        if (is.function(updateProgress)) {
          updateProgress(detail = "Fetching Sample Tags")
        }

        st_path <- CytobankAPI::sample_tags.download(cyto_session(), exp_id,
                                                     directory = "./downloaded")

        st_table <- read.table(st_path, header = T, sep = '\t', stringsAsFactors = F)

        #print(st_table)
        exp_info <- list('exp_id' = exp_id, # experiment id
                    'exp_pops' = exp_pops, # populations and how to obtain them
                    'exp_comps' = exp_comps, # compensations
                    'exp_gates' = exp_gates, # gating paramters
                    'exp_lut' = exp_lut,  # scales and transformations
                    'exp_fcs' = exp_fcs, # filenames
                    'exp_tags' = st_table) # sampletags
        return(exp_info)
      }
    )

  output$exper_info <- reactive({
    verbatimTextOutput(experiment_info())
  })


   api_exp <- reactive({
     return(list('mode' = 'api',
                 'cyto_session' = cyto_session(),
                 'exp_info' = experiment_info(),
                 'timeout' = input$cytobank_timeout))
   })

  return(api_exp)
}
