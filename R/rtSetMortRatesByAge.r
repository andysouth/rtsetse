#' setting age specific mortality rates for Tsetse
#'
#' \code{rtSetMortRatesByAge} 
#' creates a vector of mortality rates by Age.
#' Gets length of vector from vPop.
#' Needs to be called separately for males & females.
#' \cr As a first shot I have set defaults in between those for M&F.

#' @param vPop a vector of age distribution (only used to get length)
#' @param pMortAge1 mortality rate at day 1 (the maximum)
#' @param iMortMinAgeStart  Age at which min death rates start. 
#' @param iMortMinAgeStop   Age at which min death rates stop.
#' @param fMortMinProp  What proportion of the maximum death rate on day 0 is the minimum death rate.
#' @param fMortOldProp  What proportion of the maximum death rate on day 0 is the death rate after iDeathMinAgeStop.
#' 
#' @return a vector of mortality probabilities
#' @examples
#' vpMorts <- rtSetMortRatesByAge(c(1:100))
#' @export

rtSetMortRatesByAge <- function( vPop, 
                                       pMortAge1 = 0.14,
                                       iMortMinAgeStart = 10,
                                       iMortMinAgeStop = 50,
                                       fMortMinProp = 0.2,
                                       fMortOldProp = 0.3 )
{
  
  #todo: pass iMaxAge rather than vPop to this
  
  #create a vector of ages
  vAges <- seq(vPop)
  
  #create a vector filled with NAs
  vpMort <- rep(NA,length(vAges))
  
  #set day1 mortality
  vpMort[1] <- pMortAge1
  #!tricky bit is setting mortality from day2 to iMortMinAgeStart-1
  #it is a linear relationship
    
  #! this works but is horrible !
  #! there must be a simpler way of doing
  #! but I might not need to
  viEarlyAges <- c(1:(iMortMinAgeStart-1))   
  #using y=gradient*x +intercept
  gradient = -(pMortAge1 - (pMortAge1*fMortMinProp))/length(viEarlyAges)
  intercept = pMortAge1
  #the 1+ early on is because I used day1 rather than day0 for the intercept
  vpMort[1+viEarlyAges] <- (viEarlyAges)*gradient + intercept 
  
  #use plot(vpMort) to test
  
  #set mortalities in the minimum period (mature adults)
  #to MortAge1 multiplied by the minimum mortality proportion
  vpMort[iMortMinAgeStart:iMortMinAgeStop] <- pMortAge1 * fMortMinProp
  #set mortalities in the old period
  vpMort[(1+iMortMinAgeStop):length(vAges)] <- pMortAge1 * fMortOldProp
  
  invisible(vpMort)
}