#' tsetse mortality
#'
#' \code{rtMortality} returns a list of the age distributions of males & females
#' ?Should this advance time, maybe not.
#' ?there could be an option to do F only if only F are passed
#' !It should check that the length of the age structure and pMort vectors are the same
#' It uses the length of the age structure vectors passed to it
#' 1) \cr
#' 2) \cr

#' @param vFem a vector of the age distribution of Females 
#' @param vMal a vector of the age distribution of Males 
#' @param vpMortF a vector of age-specific mortality probabilities of Females 
#' @param vpMortM a vector of age-specific mortality probabilities of Males 
#' 
#' @return a list containing vFem & vMal
#' @export

rtMortality <- function( vFem                                
                       , vMal
                       , vpMortF
                       , vpMortM )
{

  #I can just replace values in the existing vectors
  
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