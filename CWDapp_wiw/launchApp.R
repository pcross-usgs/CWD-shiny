#' launches the shinyCWDApp
#' @param example name of the application to run
#' 
#' 
#' @export
#'

# wrapper for shiny::shinyApp()
launchApp <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "CWDsims"))
  
  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  
  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `launchApp()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  
  appDir <- system.file("shiny-examples", example, package = "CWDsims")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CWDsims`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
