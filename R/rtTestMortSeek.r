#' testing seeking mortality that generates stability
#'
#' \code{rtTestMortSeek} 
#' early development

#' @param iMaxAge max age of fly allowed in model
#' @param fPopAge1 starting popn at age1
#' @param fInterval the interval between mortalities used to test
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
#' @param plot whether to plot graphs
#'  
#' @return float mortality probability
#' @export


rtTestMortSeek <- function( iMaxAge = 100,
                            fPopAge1 = 100,
                            fInterval = 0.001,
#                             iMortMinAgeStart = 10,
#                             iMortMinAgeStop = 50,
#                             fMortMinProp = 0.2,
#                             fMortOldProp = 0.3,
#                             propMortAdultDD = 0.25,
#                             pMortPupa = 0.25,
#                             propMortPupaDD = 0.25,
#                             iPupDurF = 26,
#                             iPupDurM = 28,
#                             iFirstLarva = 16,
#                             iInterLarva = 10,
                            pMortLarva = 0.05,
#                             propMortLarvaDD = 0.25,
                            plot =  TRUE )
{
  #create a vector of mortalities to test
  #vMorts <- seq(fInterval, 1, fInterval) 
  #unlikely to ever go above mort 0.4 (or even get close to)
  vMorts <- seq(fInterval, 0.4, fInterval) 
  
  #plot a graph of larvae output ac/to mortality
#   vLarvae <- mapply(rtTestMortOne, fMort=vMorts, fPopAge1=fPopAge1, iMaxAge=iMaxAge)
#   plot( vMorts, vLarvae, type='l', ylab='larvae' )
#   #add starting pop age1
#   abline(h=fPopAge1, col='red')
  
  #so I want to find the mortality level that 
  #causes num larvae to equal the popAge1
  #!be careful of gender issues, may need to divide by 2
  
  #one way to work out is just to find the least magnitude value
  #of vLarvae-fPopAge1
#   bestMortIndex = which.min(abs(vLarvae-fPopAge1))
#   bestMort <- vMorts[bestMortIndex]
#   cat("bestMort:",bestMort,"larvae:",vLarvae[bestMortIndex],"\n")
  
  #This should be a quicker way of doing (but doesn't allow whole plot)
  #Because increasing mortality always decreases num pupae produced, 
  #I can actually start low, and stop when I reach the target num pupae.
  ##i.e. don't have to do for full range of mortalities
  #create a vector filled with NAs
  vLarvae2 <- rep(NA,length(vMorts))
  
  larvae <- fPopAge1+1
  trial <- 1
  while( larvae > fPopAge1 )
  {
    larvae <- rtTestMortOne( fMort=vMorts[trial], fPopAge1=fPopAge1, iMaxAge=iMaxAge, pMortLarva=pMortLarva )
    vLarvae2[trial] <- larvae
    trial <- trial+1
  }
  #set bestMort to the final trial (where larvae>=fPopAge)
  bestMort <- vMorts[trial-1]
  cat("bestMort2:",bestMort,"larvae:",larvae,"\n")
  #add to the plot
  #lines( vMorts, vLarvae2, col='blue', lty='dotted', lwd=2 )
  
  #2nd version when not adding to all values
  if (plot)
  {
    plot( vMorts, vLarvae2, type='l', ylab='larvae' )
    #add starting pop age1
    abline(h=fPopAge1, col='red')    
  }


  
  bestMort
}

  