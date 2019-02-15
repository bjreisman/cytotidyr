#' Runs the shiny gui for cytotidyr
#'
#' @export
#' @import shiny
#'

cytotidyr_gui <- function() {
  appDir <- system.file("shiny-apps", "cytotidyr_gui", package = "cytotidyr")
  if (appDir == "") {
    stop("Could not find shiny-apps directory. Try re-installing `cytotidyr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
