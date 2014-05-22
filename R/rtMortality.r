#' tsetse mortality
#'
#' \code{rtMortality} returns a list of the age distributions of males & females.
#' \cr It accepts age and sex specific mortality probabilities.
#' \cr !It should check that the length of the age structure and pMort vectors are the same
#' \cr It uses the length of the age structure vectors passed to it.
#' \cr There could be an option to do F only if only F are passed.

#' @param vFem a vector of the age distribution of Females 
#' @param vMal a vector of the age distribution of Males 
#' @param vpMortF a vector of age-specific mortality probabilities of Females 
#' @param vpMortM a vector of age-specific mortality probabilities of Males 
#' @param propDD proportion of mortality that is density dependent 
#' @param iCarryCap Carrying Capacity as an integer
#' 
#' @return a list containing vFem & vMal
#' @export

rtMortality <- function( vFem,                               
                         vMal,
                         vpMortF,
                         vpMortM,
                         propDD = 0.25,
                         iCarryCap = NA ) #?not sure whether to provide a default for iCarryCap
{

  #simply replaces values in the passed vectors before returning
  
  ###################
  #density dependence
  #different to pupal & larval mortality in that dd needs to be
  #applied to a vector of age-specific mortality probabilities
  #surprisingly it seems to work on vectors as well as single values
  if ( propDD > 0 )
  {
    vpMortF <- rtDensityDependence( fPopn= (sum(vFem)+sum(vMal)),
                                    pMort = vpMortF,
                                    propDD = propDD,
                                    iCarryCap = iCarryCap )

    vpMortM <- rtDensityDependence( fPopn= (sum(vFem)+sum(vMal)),
                                    pMort = vpMortM,
                                    propDD = propDD,
                                    iCarryCap = iCarryCap )
  }

  
  
  
  #!I should be able to do this in a vectorised way rather than using loops. 
  
  #use seq_len(N) instead of 1:N (it handles the N<1 case appropriately)
  #for( age in 1:length(vFem) )
  for( age in seq_along(vFem) )  
  {
    vFem[age] <- vFem[age] * (1-vpMortF[age])   
  }
  
  #for( age in 1:length(vMal) )
  for( age in seq_along(vMal) )  
  {
    vMal[age] <- vMal[age] * (1-vpMortM[age])   
  }
  
  #returning a list of the new age structures
  invisible( list(vFem=vFem, vMal=vMal) )
  
} #end of rtMortality()