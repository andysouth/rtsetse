#' tsetse feeding for one cell,age,sex and hunt period
#' 
#' \code{rtFeedingOneHuntPeriod} does feeding for one group of flies in one hunt period.
#' In the Hat-trick model there were a variable number of hunt periods per day (15-30).
#' In rtPhase3 the initial proposal is for 15 hunt periods on day 1 & 30 hunt periods on later hunger cycles. 

#' @param fHunters the number of hunting flies in this cell,age,sex and hunt period
#' @param pDetectMan probability of detecting one human per km2
#' @param pDetectOxe probability of detecting one other host (ox equivalent) per km2
#' @param pFeedMan probability of feeding on a human once detected 
#' @param pFeedOxe probability of feeding on other host (ox equivalent) once detected 
#' @param fDensityMan density of humans per km2 in tsetse habitat 
#' @param fDensityOxe density of other hosts (ox equivalents) per km2 in tsetse habitat 
#' @param testing whether to output testing messages to the console 
#' 
#' @return a list of fManFeeders and ?fHunters 
#' @examples
#' #if both probs set to 1, returns all feeders
#' rtFeedingOneHuntPeriod(100,pDetectMan=1,pDetectOxe=0,pFeedMan=1,pFeedOxe=1)
#' #if both probs set to 0, returns all hunters 
#' rtFeedingOneHuntPeriod(100,pDetectMan=0,pDetectOxe=0,pFeedMan=1,pFeedOxe=1)
#' @export

rtFeedingOneHuntPeriod <- function( fHunters=1000,
                           pDetectMan=0.001,
                           pDetectOxe=0.005,
                           pFeedMan=0.1,
                           pFeedOxe=0.8,
                           fDensityMan=1,
                           fDensityOxe=10,
                           testing = TRUE ) 
{
  
              
  #detection
  fManDetectors <- fHunters * pDetectMan
  fOxeDetectors <- fHunters * pDetectOxe

  #need to add something about
  #detecting man before ox or vice-verca
  
  #feeding
  fManFeeders <- fManDetectors * pFeedMan
  fOxeFeeders <- fOxeDetectors * pFeedOxe
  
  #reducing the hunters by those that fed
  fHunters <- fHunters - (fManFeeders + fOxeFeeders) 
  
  
  
  #returning a list of aGridStarved & aGridManFeeders
  #invisible( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  return( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  
  
  
  
} 