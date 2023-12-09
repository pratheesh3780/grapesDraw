#' grapesDraw
#'
#' @return Nothing
#' @import RColorBrewer
#' @import corrplot
#' @import dplyr
#' @import factoextra
#' @import ggridges
#' @import ggthemes
#' @import grDevices
#' @import shinyWidgets
#' @import shinycssloaders
#' @import ggplot2
#' @import shiny
#' @import shinydashboard
#' @import shinytest
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#' draw()
#' }
draw <- function() {
  appFile <- system.file("draw", package = "grapesDraw")
  if (!file.exists(appFile)) {
    stop("Could not find the Shiny app file. Try re-installing `grapesDraw`.", call. = FALSE)
  }

  shiny::runApp(appFile, display.mode = "normal")
}
