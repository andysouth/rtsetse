#' testing seeking mortality that generates stability
#'
#' \code{rtTestMortOne} 
#' early development
#' @param fMort test age-independent mortality
#' @param iMaxAge max age of fly allowed in model
#' @param fPopAge1 starting popn at age1

#' @return num larvae
#' @export

#rtTestMortSeek <- Vectorize( function( fMort,
rtTestMortOne <- function( fMort,
                            iMaxAge = 100,
                            fPopAge1 = 100 )
{
 
  #iMaxAge <- 100
  
  #set mortality by age (& determines num ages)
  #vpMort <- rep(0.1,50)
  vpMort <- rep(fMort, iMaxAge)
  
  #fill an age structure
  vPop <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=fPopAge1 )
  
  #!beware this uses a bunch of defaults
  #and just passes vPop twice for M&F for DD
  #DD set to NA as default so prob doesn't matter
  vpDeposit <- rtSetDepositionRatesByAge( vPop, vPop )
  
  #use the deposition rates set above
  lLarvae <- rtLarvalDeposition( vPop, vpDeposit )    
  #summing M&F
  iLarvae <- lLarvae$iLarvaeF + lLarvae$iLarvaeM  
  
  #returning num larvae
  iLarvae
}
#})
  