#' simulating tsetse fly populations to investigate control options.
#'
#' rtsetse is being developed to provide functions to create tsetse fly population simulations with age, sex and spatial structure.  
#'
#' The main functions to run simulations are :
#'  \describe{
#'   \item{rt_runAspatial()}{age-structured a-spatial population simulation}
#'   \item{rt_runGridTestSpread()}{grid simulation, homogenous landscape, spread fro central populated cell}
#'   \item{rt_runGrid()}{grid simulation, grid simulation, landscape set from vegetation map, population starts at carrying capacity}
#' }
#' 
#' @author
#' Andy South \email{southandy@@gmail.com} 

#' @import raster sp ggplot2
#' @importFrom dplyr group_by summarise %>%
#' @docType package
#' @name rtsetse
#' @seealso 
#'  \url{http://andysouth.shinyapps.io/shinytse7/} demonstration user interface   
#'  \url{http://andysouth.github.io/rtsetse-intro/} blog post outlining the simulation   
#'  \url{http://andysouth.github.io/grid-movement/} blog post decsribing how movement is simulated   
NULL