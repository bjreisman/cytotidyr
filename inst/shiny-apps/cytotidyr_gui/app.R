#appDir <- system.file("shiny-apps", "cytotidyr_gui", package = "cytotidyr")
appDir <- getwd()
#list.files(appDir)
#source("./R/loadfiles.R")
source(file.path(appDir, "R", "loadfiles.R"))
library(shiny)
###############################################################################
ui <- navbarPage(title = 'CytoTidyr', id = "mainNavbarPage",
                 tabPanel(title = "Connect to Cytobank",
                          id = 'tab1',
                          value = 'tab1',
                          fluidRow(
                              column(4),
                              column(4,
                                wellPanel(api_mode_ui('api_mode')),
                                verbatimTextOutput('mytext1')
                               ),
                               column(4))
                 ),
                 tabPanel('title' = "Select Experiment",
                          id = 'tab2',
                          value = 'tab2',
                          fluidRow(
                            column(6, wellPanel(experiment_list_ui('experiment_list'))),
                            column(6, wellPanel(importbyfcsfile_ui('importbyfcsfile')))
                            )
                 ),
                 tabPanel('title' = 'Preprocessing',
                          id = 'tab3',
                          value = 'tab3',
                          fluidRow(

                          ))
)

server <- function(input, output, session){
  api_connection <- callModule(api_mode, 'api_mode', x = session)

  exp_info_reactive <-
    callModule(experiment_list, 'experiment_list', api_connection, x = session)

  importbyfcsfile_reactive <-
    callModule(importbyfcsfile,
               'importbyfcsfile',
               api_connection,
               exp_info_reactive,
               x = session)
#
#   output$mytext1 <- renderText(
#     names(exp_info_reactive())
#   )

 # selected_data <- callModule(selectdata , 'selectdata', mysetup, x = session)
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
