#' @importFrom shiny runApp
#' @export
shiny.likert <- function(){
  appDir <- system.file("shinyApp", package = 'shiny.likert' )
  shiny::runApp(appDir)
}
