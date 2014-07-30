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
#' #can use this one to compare with the table outputs from Excel
#' rtFeedingOneHuntPeriod(1000,pDetectMan=0.001,pDetectOxe=0.005,pFeedMan=0.1,pFeedOxe=0.8,fDensityMan=1,fDensityOxe=10)
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

  ##some argument checking
  if( pDetectMan < 0 | pDetectMan > 1 )
    stop("pDetectMan needs to be between 0 & 1 it is ",pDetectMan)
  if( pDetectOxe < 0 | pDetectOxe > 1 )
    stop("pDetectOxe needs to be between 0 & 1 it is ",pDetectOxe)  
  if( pFeedMan < 0 | pFeedMan > 1 )
    stop("pFeedMan needs to be between 0 & 1 it is ",pFeedMan)
  if( pFeedOxe < 0 | pFeedOxe > 1 )
    stop("pFeedOxe needs to be between 0 & 1 it is ",pFeedOxe)  
  
  
  #detection
  #tricky to work out prob of first detected host being of type
  #(Hat-trick NittyGritty Table1 HIJK)
  pNotDetectMan <- (1-pDetectMan)^fDensityMan
  pNotDetectOxe <- (1-pDetectOxe)^fDensityOxe
  pNotDetectAny <- pNotDetectMan * pNotDetectOxe
  pDetectAny <- 1-pNotDetectAny

  #probability of first detected host being of type
  #??I'm not sure about Glyn's logic here
  #??I wonder of it should be power rather than multiplication
  #??or if it matters
  fDetTimesDenseMan <- pDetectMan*fDensityMan
  fDetTimesDenseOxe <- pDetectOxe*fDensityOxe
  #(Hat-trick NittyGritty Table1 L&M)
  #add in protection for zeroes to avoid returning NaN
  #probability of first detected host being human
  if (fDetTimesDenseMan==0) pFirstDetectMan <- 0
  else pFirstDetectMan <- fDetTimesDenseMan / (fDetTimesDenseMan+fDetTimesDenseOxe)
  
  #Glyn did it this way, I did diferently in case no Oxe
  #pFirstDetectOxe <- 1-pFirstDetectMan
  #probability of first detected host being other than human
  if (fDetTimesDenseOxe==0) pFirstDetectOxe <- 0
  else pFirstDetectOxe <- fDetTimesDenseOxe / (fDetTimesDenseMan+fDetTimesDenseOxe)
  
  
  
  fManDetectors <- fHunters*pDetectAny*pFirstDetectMan
  fOxeDetectors <- fHunters*pDetectAny*pFirstDetectOxe
  
  #feeding 
  #(Hat-trick NittyGritty Table2 C)
  fManFeeders <- fManDetectors * pFeedMan 
  fOxeFeeders <- fOxeDetectors * pFeedOxe
  
  #this is done slightly differently to Glyn, but should give same
  
  #reducing the hunters by those that fed
  fHunters <- fHunters - (fManFeeders + fOxeFeeders) 
  
  
  #returning a list of aGridStarved & aGridManFeeders
  #invisible( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  return( list(fManFeeders=fManFeeders, fHunters=fHunters) )
  
  
  
  
} 