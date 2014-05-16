#' setting age specific larval deposition rates for Tsetse
#'
#' \code{rtSetDepositionRatesByAge} 
#' 
#' Gets length of vector from vPopF
#' uses age of first larva, interlava period & larval mortality rate 
#' to create a vector of deposition rates.

#' @param vPopF a vector of the age distribution of Females 
#' @param vPopM a vector of the age distribution of Males, needed to assess density dependence 
#' @param iFirstLarva age that first larva deposited 
#' @param iInterLarva gap between deposition of larvae 
#' @param pMortLarva larval mortality rate 
#' @param propMortDD proportion of mortality that is density dependent, set to 0 as default 
#' @param iCarryCap Carrying Capacity as an integer, not needed if propMortDD=0
#' 
#' @return a vector of larval deposition probabilities
#' @export

rtSetDepositionRatesByAge <- function( vPopF, 
                                       vPopM,
                                       iFirstLarva = 16,
                                       iInterLarva = 10,
                                       pMortLarva = 0.05,
                                       propMortDD = 0,
                                       iCarryCap = NA )
{
  
  #test1 all F set to 1
  #vpDeposit <- rep(1,length(vPopF))
  #test2 F>age5 set to 1
  #vpDeposit <- ifelse(seq(vPopF)>5,1,0)
  #test3 F>age5 set to 0.5
  #vpDeposit <- ifelse(seq(vPopF)>5,0.5,0)
  
  #create a vector of ages
  vAges <- seq(vPopF)
  
  #Depositors at ages 16, 16+10, 16+20 etc
  #this does that using modulus
  vpDeposit <- ifelse( vAges==iFirstLarva | ((vAges-iFirstLarva)>0 & (vAges-iFirstLarva)%%iInterLarva==0),1,0)
  
  #density dependence
  #setting propMortDD to 0 can stop density dependence being implemented
  if ( propMortDD > 0 )
    pMortLarva <- rtDensityDependence( fPopn = (sum(vPopF)+sum(vPopM)),
                                  pMort = pMortLarva,
                                  propDD = propMortDD,
                                  iCarryCap = iCarryCap )
  
  #impose larval death rate
  vpDeposit <- vpDeposit * (1-pMortLarva)
  
  invisible(vpDeposit)
}