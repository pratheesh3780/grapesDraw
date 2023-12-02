
# function to initialize grapesDraw
draw <- function() {
  appFile <- system.file("app.R", package = "grapesDraw")
  if (!file.exists(appFile)) {
    stop("Could not find the Shiny app file. Try re-installing `grapesDraw`.", call. = FALSE)
  }

  shiny::runApp(appFile, display.mode = "normal")
}
