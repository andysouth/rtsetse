#' run a shiny User Interface to the model
#' 
#' displays a user interface in the browser, allowing users to run the model

#' @return nothing
#' @export

rt_UI <- function() {
  
  #launch.browser=TRUE to make sure downloads of results work
  #also makes exiting the UI cleaner, i.e. no errors
  
  shiny::runApp(system.file('shiny/shinytsetse', package='rtsetse'), launch.browser=TRUE)
}