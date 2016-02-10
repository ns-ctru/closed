#' Runs Shiny web-server for display the results
#'
#' @export
nmisc_example <- function() {
    # locate all the shiny app examples that exist
  appDir <- system.file("shiny", "closed", package = "closed")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
