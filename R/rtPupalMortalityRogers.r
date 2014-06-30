#' tsetse pupal mortality following Rogers(1990)
#'
#' \code{rtPupalMortalityRogers} applies pupal mortality following method of Rogers(1990).
#' \cr Requires 3 parameters, the density independent mortality, threshold that density dependence starts and it's slope (k).
#' \cr Developed in discussion with John Hargrove
#' 
#' @param vPupaF a vector of female pupae by age 
#' @param vPupaM a vector of male pupae by age 
#' @param pMort a density independent mortality probability 
#' @param iPupaDensThresh the threshold above which density dependence acts
#' @param fSlopeDD the slope of density dependence, how mortality increases with density
#' 
#' @return a list of vectors of pupae by age (vPupaF & vPupaM)
#' @export

rtPupalMortalityRogers <- function( vPupaF,
                              vPupaM,
                              pMort = 0.25,
                              iPupaDensThresh = 200,
                              fSlopeDD = 1.0 ) 
  
{  
  
  fTotPupa <- sum(vPupaF) + sum(vPupaM)
  
  #density dependence only if pupal density is greater than the threshold
  #if ( fTotPupa > iPupaDensThresh )
  #  pMort <- pMort + fSlopeDD
  
  pMort <- rtDDRogers( fPop=fTotPupa, pMort=pMort, iPupaDensThresh=iPupaDensThresh, fSlopeDD=fSlopeDD )
  
  #pupal mortality is applied once during the pupal period
  #done on day 1
  vPupaF[1] <- vPupaF[1] * (1-pMort) 
  vPupaM[1] <- vPupaM[1] * (1-pMort) 
  
  #return the modified vector
  invisible( list(vPupaF=vPupaF,vPupaM=vPupaM) ) 
  
}

#' add density dependence to a mortality following Rogers(1990)
#'
#' \code{rtDDRogers} applies pupal mortality following method of Rogers(1990).
#' \cr Requires 3 parameters, the density independent mortality, threshold that density dependence starts and it's slope (k).
#' \cr Developed in discussion with John Hargrove
#' 
#' @param fPopn current popn size 
#' @param pMort a density independent mortality probability 
#' @param iPupaDensThresh the threshold above which density dependence acts
#' @param fSlopeDD the slope of density dependence, how mortality increases with density
#' 
#' @return a mortality rate with density dependence added
#' @export
rtDDRogers <- function( fPopn = 200,
                        pMort = 0.25,
                        iPupaDensThresh = 200,
                        fSlopeDD = 1.0 )
{  
  
  #density dependence only if pupal density is greater than the threshold
  if ( fPopn > iPupaDensThresh ){
    
    #log version
    #pMort <- pMort + (fSlopeDD*log(fPopn-iPupaDensThresh))
    #non log
    pMort <- pMort + (fSlopeDD*(fPopn-iPupaDensThresh))
    if(pMort > 1) pMort <- 1
    
  }
  pMort
}

  

#' tests Rogers pupal mortality and outputs a plot
#'
#' \code{rtDDRogersTest} passes a vector of densities to the density dependence function
#' along with a carrying capacity and outputs a plot of how the mortality rate
#' changes by relative density. The default parameters show an example.
#' This function is not used in the running of the simulation.
#'
#' @param vDensities a vector of densities to test
#' @param iCarryCap Carrying Capacity as an integer
#' @param propDD proportion of mortality that is density dependent 
#' @param pMort a mortality probability 
#' 
#' @return a dataframe containing densities, relative densities and resulting mortality rates
#' @export

rtDDRogersTest <- function( vDensities = c(150:300),
                            pMort = 0.25,
                            iPupaDensThresh = 200,
                            fSlopeDD = 1.0 ) 
  
{ 
  
  
  
  results <- data.frame(mortality=unlist(lapply( vDensities, function(x)  rtDDRogers(fPopn=x, pMort=pMort, iPupaDensThresh=iPupaDensThresh, fSlopeDD=fSlopeDD))))
  results$density <- vDensities
  results$relDensity <- vDensities / iPupaDensThresh
  
  plot(y=results$mortality,x=results$relDensity,type='l')
  abline(h=pMort,v=1,col='blue',lty='dotted')
  
  invisible(results)
}

