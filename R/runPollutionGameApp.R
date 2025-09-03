#' @export
runPollutionGameApp <- function() {
  appDir <- system.file("shiny-examples", "pollutionGameApp", package = "econGame365")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `econGame`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
