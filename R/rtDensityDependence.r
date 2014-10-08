#' modifies mortality according to density dependence
#'
#' \code{rtDensityDependence} takes passed mortality rate and modifies it
#' according to the relationship between density and carrying capacity.
#' Implemented in the same way as hat-trick.
#' Allows the proportion of mortality that is density dependent to be specified.
#' \cr pMort at Carrying Capacity is left unmodified.
#' \cr pMort at density 0 = pMort*(1-proportion of mortality density dependent).
#' \cr In between is a linear relationship that continues to give higher values above carrying capacity.
#' \cr ! I wonder if density dependence could be implemented in a more standard way closer to the logistic model.
#'
#' @param fPopn population number, can be adults or pupae
#' @param pMort a mortality probability 
#' @param propDD proportion of mortality that is density dependent 
#' @param iCarryCap Carrying Capacity as an integer
#' 
#' @return pMort modified mortality rate
#' @export

rtDensityDependence <- function( fPopn,
                                 pMort,
                                 propDD,
                                 iCarryCap )
  
{  

  #mortality at density 0 = pMort*(1-propDD)
  #mortality at density SCC = pMort  
  #therefore pMort = pMort*(1-propDD) + pMort*propDD*(density/iCarryCap)
  
  #to trap problems if popn or cc are 0
  if ( fPopn==0 | iCarryCap==0 ) pMort <- rep(1,length(pMort))
  
  else pMort <- pMort*(1-propDD) + pMort*propDD*(fPopn/iCarryCap)
  
  return(pMort)
}



#' tests density dependence and outputs a plot
#'
#' \code{rtDDTest} passes a vector of densities to the density dependence function
#' along with a carrying capacity and outputs a plot of how the mortality rate
#' changes by relative density. The default parameeters show an example.
#' This function is not used in the running of the simulation.
#'
#' @param vDensities a vector of densities to test
#' @param iCarryCap Carrying Capacity as an integer
#' @param propDD proportion of mortality that is density dependent 
#' @param pMort a mortality probability 
#' 
#' @return a dataframe containing densities, relative densities and resulting mortality rates
#' @export

rtDDTest <- function( vDensities = c(0:20),
                   iCarryCap = 10,
                   propDD = 0.25,
                   pMort = 0.2 )
  
{ 

  results <- data.frame(mortality=unlist(lapply( vDensities, function(x)  rtDensityDependence(fPopn=x,iCarryCap=iCarryCap,propDD=propDD,pMort=pMort))))
  results$density <- vDensities
  results$relDensity <- vDensities / iCarryCap

  plot(y=results$mortality,x=results$relDensity,type='l')
  abline(h=pMort,v=1,col='blue')
  
  invisible(results)
}

