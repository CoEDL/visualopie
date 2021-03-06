#' @export
#' This function runs the visualopie app
#'
run_visualopie <- function() {
  appDir <- system.file("shiny-examples", "visualopie", package = "visualopie")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `visualopie`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
