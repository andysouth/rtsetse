#' setting age specific density-INdependent larval deposition rates for Tsetse
#'
#' \code{rtSetDepositionRatesByAgeDI} sets age dependent larval deposition rates that are the 
#' same for all females in the popn. and do not differ by density or location. 
#' Compatible with the grid model.   
#' 
#' see \code{\link{rtSetDepositionRatesByAge}} for a deprecated density-dependent version.
#' The previous version used the length of the vector of females to get the maximum age, 
#' this version needs the iMaxAge argument.


#' @param iMaxAge the maximum age of flies, females 
#' @param iFirstLarva age that first larva deposited 
#' @param iInterLarva gap between deposition of larvae 
#' @param pMortLarva larval mortality rate 
#' 
#' @return a vector of larval deposition probabilities
#' @export

rtSetDepositionRatesByAgeDI <- function( iMaxAge = 120,
                                       iFirstLarva = 16,
                                       iInterLarva = 10,
                                       pMortLarva = 0.05 )
{
  
  #test1 all F set to 1
  #vpDeposit <- rep(1,length(vPopF))
  #test2 F>age5 set to 1
  #vpDeposit <- ifelse(seq(vPopF)>5,1,0)
  #test3 F>age5 set to 0.5
  #vpDeposit <- ifelse(seq(vPopF)>5,0.5,0)
  
  #create a vector of ages
  vAges <- seq(iMaxAge)
  
  #Depositors at ages 16, 16+10, 16+20 etc
  #this does that using modulus
  vpDeposit <- ifelse( vAges==iFirstLarva | ((vAges-iFirstLarva)>0 & (vAges-iFirstLarva)%%iInterLarva==0),1,0)
  
  #REMOVED density dependence
  #setting propMortDD to 0 can stop density dependence being implemented
#   if ( propMortDD > 0 )
#     pMortLarva <- rtDensityDependence( fPopn = (sum(vPopF)+sum(vPopM)),
#                                   pMort = pMortLarva,
#                                   propDD = propMortDD,
#                                   iCarryCap = iCarryCap )
  
  #impose larval death rate
  vpDeposit <- vpDeposit * (1-pMortLarva)
  
  invisible(vpDeposit)
}