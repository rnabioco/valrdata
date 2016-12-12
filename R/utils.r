#' launch valrdata shiny demo
#'
#' @export
launch_valr_app <- function() {
  #http://deanattali.com/2015/04/21/r-package-shiny-app/
  appDir <- system.file("valr-examples", package = "valrdata")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `valrdata`.", call. = FALSE)
  }

  shiny::runApp(appDir)
}
