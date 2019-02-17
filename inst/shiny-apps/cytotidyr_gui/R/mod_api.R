

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
        updateNavbarPage(x, "mainNavbarPage", "tab2")
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


  api_connection <- reactive({

     return(list('mode' = 'api',
                 'cyto_session' = cyto_session(),
                 'timeout' = input$cytobank_timeout,
                 'check_if_connected' = check_if_connected_reactive())
     )
   })

  return(api_connection)
}

shinyApp(api_mode_ui, api_mode)
