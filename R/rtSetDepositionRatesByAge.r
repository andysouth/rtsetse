#' setting Age Specific Larval Deposition rates for Tsetse
#'
#' \code{rtSetDepositionRatesByAge} 
#' 
#' Gets length of vector from vFem
#' uses age of first larva, interlava period & larval mortality rate 
#' to create a vector of deposition rates

#' @param vFem a vector of the age distribution of Females 
#' @param iFirstLarva age that first larva deposited 
#' @param iInterLarva gap between deposition of larvae 
#' @param pMortLarva larval mortality rate 
#' 
#' @return a vector of larval deposition probabilities
#' @export

rtSetDepositionRatesByAge <- function( vFem,                                
                                       iFirstLarva = 16,
                                       iInterLarva = 10,
                                       pMortLarva = 0.05 )
{
  
  #test1 all F set to 1
  #vpDeposit <- rep(1,length(vFem))
  #test2 F>age5 set to 1
  #vpDeposit <- ifelse(seq(vFem)>5,1,0)
  #test3 F>age5 set to 0.5
  #vpDeposit <- ifelse(seq(vFem)>5,0.5,0)
  
  #create a vector of ages
  vAges <- seq(vFem)
  
  #Depositors at ages 16, 16+10, 16+20 etc
  #this does that using modulus
  vpDeposit <- ifelse( vAges==iFirstLarva | ((vAges-iFirstLarva)>0 & (vAges-iFirstLarva)%%iInterLarva==0),1,0)
  
  #impose larval death rate
  vpDeposit <- vpDeposit * (1-pMortLarva)
  
  invisible(vpDeposit)
}