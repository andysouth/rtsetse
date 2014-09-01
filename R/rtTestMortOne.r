#' testing seeking mortality that generates stability
#'
#' \code{rtTestMortOne} 
#' early development
#' @param fMort test age-independent mortality
#' @param iMaxAge max age of fly allowed in model
#' @param fPopAge1 starting popn at age1
#' @param iMortMinAgeStart  Age at which min death rates start. 
#' @param iMortMinAgeStop   Age at which min death rates stop.
#' @param fMortMinProp  What proportion of the maximum death rate on day 0 is the minimum death rate.
#' @param fMortOldProp  What proportion of the maximum death rate on day 0 is the death rate after iDeathMinAgeStop.
#' @param propMortAdultDD proportion of adult mortality that is density dependent
#' @param pMortPupa pupal mortality per period
#' @param propMortPupaDD proportion of pupal mortality that is density dependent
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(M) to develop
#' @param iFirstLarva Age that female produces first larva
#' @param iInterLarva Inter-larval period
#' @param pMortLarva larval mortality per period
#' @param propMortLarvaDD proportion of larval mortality that is density dependent


#' @return num larvae
#' @export

#rtTestMortSeek <- Vectorize( function( fMort,
rtTestMortOne <- function( fMort,
                            iMaxAge = 100,
                            fPopAge1 = 100,
                           
#                            iMortMinAgeStart = 10,
#                            iMortMinAgeStop = 50,
#                            fMortMinProp = 0.2,
#                            fMortOldProp = 0.3,
#                            propMortAdultDD = 0.25,
#                            pMortPupa = 0.25,
#                            propMortPupaDD = 0.25,
#                            iPupDurF = 26,
#                            iPupDurM = 28,
#                            iFirstLarva = 16,
#                            iInterLarva = 10,
                            pMortLarva = 0.05 )
#                            propMortLarvaDD = 0.25)
{
 
  #iMaxAge <- 100
  
  #set mortality by age (& determines num ages)
  #this is an early test, later mortality will be age dependent
  #vpMort <- rep(0.1,50)
  vpMort <- rep(fMort, iMaxAge)
  
  #fill an age structure
  vPop <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=fPopAge1 )
  
  #!beware this uses a bunch of defaults
  #and just passes vPop twice for M&F for DD
  #DD set to NA as default so prob doesn't matter
  vpDeposit <- rtSetDepositionRatesByAge( vPop, vPop, pMortLarva=pMortLarva )
  
  #use the deposition rates set above
  lLarvae <- rtLarvalDeposition( vPop, vpDeposit )    
  #summing M&F
  iLarvae <- lLarvae$iLarvaeF + lLarvae$iLarvaeM  
  
  #returning num larvae
  iLarvae
}
#})
  