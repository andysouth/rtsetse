#' run a shiny User Interface to the model
#' 
#' displays a user interface in the browser, allowing users to run the model

#' @return nothing
#' @export

rt_UI <- function() {
  
  shiny::runApp(system.file('shiny/shinytsetse', package='rtsetse'))
}