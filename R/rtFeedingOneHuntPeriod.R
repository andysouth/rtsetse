#' tsetse feeding for one cell,age,sex and hunt period
#' 
#' \code{rtFeedingOneHuntPeriod} does feeding for one group of flies in one hunt period.
#' In the Hat-trick model there were a variable number of hunt periods per day (15-30).
#' In rtPhase3 the initial proposal is for 15 hunt periods on day 1 & 30 hunt periods on later hunger cycles. 

#' @param fHunters the number of hunting flies in this cell,age,sex and hunt period
#' @param pDetect TEMPORARY starting arg, single probability of detection
#' @param pFeed TEMPORARY starting arg, single probability of feeding once detected 
#' @param testing whether to output testing messages to the console 
#' 
#' @return a list of fManFeeders and ?fHunters 
#' @examples
#' #if both probs set to 1, returns all feeders
#' rtFeedingOneHuntPeriod(100,1,1)
#' #if both probs set to 0, returns all hunters 
#' rtFeedingOneHuntPeriod(100,0,0)
#' @export

rtFeedingOneHuntPeriod <- function( fHunters=100,
                           pDetect=0.1,
                           pFeed=0.1,
                           testing = TRUE ) 
{
  

              
  #detection
  #               nonDetectors = hunters * probabilityX
  #               manDetectors = hunters * probabilityY
  #               oxeDetectors = hunters * probabilityZ
  fManDetectors <- fHunters * pDetect
  
  #feeding
  fManFeeders <- fManDetectors * pFeed
  #for the initial test I'm keeping oxeFeeders at zero
  fOxeFeeders <- 0 # oxeDetectors * probabilityB
  #reducing the hunters by those that fed
  fHunters <- fHunters - (fManFeeders + fOxeFeeders) 
  

  
  #accumulate manFeeders for the day
  #aGridManFeeders[x,y,sex,age] <- aGridManFeeders[x,y,sex,age] + manFeeders
              
  
  
  #returning a list of aGridStarved & aGridManFeeders
  #invisible( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  return( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  
  #! as initial test just return manFeeders
  #invisible( fManFeeders )
  
  
  
} 