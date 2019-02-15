appDir <- system.file("shiny-apps", "cytotidyr_gui", package = "cytotidyr")
#list.files(appDir)
#source("./R/loadfiles.R")
source(file.path(appDir, "R", "loadfiles.R"))

###############################################################################
ui <- fluidPage(title = 'CytoTidyr',
                          fluidRow(
                            column(12,
                              h2("CytoTidyr"),
                              h5("A tool for tidying cytometry data"),
                              hr(),
                              column(3,
                                wellPanel(api_mode_ui('api_mode'))
                               )#,
                              # column(9
                              # #  selectdata_ui('selectdata')
                              # )
                            ),
                            column(12, offset = 0,
                                 br(),
                                 hr()#,
                                 #includeHTML("./about/about_body.html")
                            )
                          )
                 )

server <- function(input, output, session){
  mysetup <- callModule(api_mode, 'api_mode', x = session)
 # selected_data <- callModule(selectdata , 'selectdata', mysetup, x = session)
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
