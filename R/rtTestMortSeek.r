#' testing seeking mortality that generates stability
#'
#' \code{rtTestMortSeek} 
#' early development

#' @param iMaxAge max age of fly allowed in model
#' @param fPopAge1 starting popn at age1
#' @param fInterval the interval between mortalities used to test
#' 
#' @return num larvae
#' @export


rtTestMortSeek <- function( 
                            iMaxAge = 100,
                            fPopAge1 = 100,
                            fInterval = 0.001 )
{
  #create a vector of mortalities to test
  #vMorts <- seq(fInterval, 1, fInterval) 
  #unlikely to ever go above mort 0.4 (or even get close to)
  vMorts <- seq(fInterval, 0.4, fInterval) 
  
  #plot a graph of larvae output ac/to mortality
  vLarvae <- mapply(rtTestMortOne, fMort=vMorts, fPopAge1=fPopAge1, iMaxAge=iMaxAge)
  plot( vMorts, vLarvae, type='l', ylab='larvae' )
  #add starting pop age1
  abline(h=fPopAge1, col='red')
  
  #so I want to find the mortality level that 
  #causes num larvae to equal the popAge1
  #!be careful of gender issues, may need to divide by 2
  
  #one way to work out is just to find the least magnitude value
  #of vLarvae-fPopAge1
  bestMortIndex = which.min(abs(vLarvae-fPopAge1))
  bestMort <- vMorts[bestMortIndex]
  
  cat("bestMort:",bestMort,"larvae:",vLarvae[bestMortIndex],"\n")
  
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
    larvae <- rtTestMortOne( fMort=vMorts[trial], fPopAge1=fPopAge1, iMaxAge=iMaxAge )
    vLarvae2[trial] <- larvae
    trial <- trial+1
  }
  cat("bestMort2:",vMorts[trial-1],"larvae:",larvae,"\n")
  #add to the plot
  lines( vMorts, vLarvae2, col='blue', lty='dotted', lwd=2 )
  
  
  bestMort
}

  