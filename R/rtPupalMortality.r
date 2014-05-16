#' tsetse pupal mortality
#'
#' \code{rtPupalMortality} applies pupal mortality once in the pupal period.
#' \cr Same rate for males and females (as in Hat-Trick).
#' \cr Initially this was called for each sex,
#' but because density dependence needs to know about both sexes I changed so that both are done in same function call.
#' \cr !Later I may need to pass vegetation type to this or another function that modifies vital rates before passing.

#' @param vPupaF a vector of female pupae by age 
#' @param vPupaM a vector of male pupae by age 
#' @param pMort a mortality probability 
#' @param propDD proportion of mortality that is density dependent 
#' @param iCarryCapPupa Carrying Capacity as an integer
#' 
#' @return a list of vectors of pupae by age (vPupaF & vPupaM)
#' @export

rtPupalMortality <- function( vPupaF,
                              vPupaM,
                              pMort = 0.25,
                              propDD = 0.25,
                              iCarryCapPupa = NA ) #?not sure whether to provide a default for iCarryCapPupa

{  
  
  #density dependence
  #setting propDD to 0 can stop an density dependence being implemented
  if ( propDD > 0 )
    pMort <- rtDensityDependence( fPopn= (sum(vPupaF)+sum(vPupaM)),
                                  pMort = pMort,
                                  propDD = propDD,
                                  iCarryCap = iCarryCapPupa )
    
  #pupal mortality is applied once during the pupal period
  #done on day 1
  vPupaF[1] <- vPupaF[1] * (1-pMort) 
  vPupaM[1] <- vPupaM[1] * (1-pMort) 
  
  #return the modified vector
  invisible( list(vPupaF=vPupaF,vPupaM=vPupaM) ) 
  
}