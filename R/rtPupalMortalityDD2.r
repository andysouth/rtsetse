#' tsetse pupal mortality using alternative density dependence mechanism
#'
#' \code{rtPupalMortalityDD2} applies pupal mortality once in the pupal period.
#' \cr Same rate for males and females (as in Hat-Trick).
#' \cr Initially this was called for each sex,
#' but because density dependence needs to know about both sexes I changed so that both are done in same function call.
#' \cr !Later I may need to pass vegetation type to this or another function that modifies vital rates before passing.

#' @param vPupaF a vector of female pupae by age 
#' @param vPupaM a vector of male pupae by age 
#' @param pMort a mortality probability 
#' @param iCarryCapPupa Carrying Capacity as an integer
#' @param fPopNow popn today 
#' @param fPopPre popn day before
#' 
#' 
#' @return a list of vectors of pupae by age (vPupaF & vPupaM)
#' @export

rtPupalMortalityDD2 <- function( vPupaF,
                              vPupaM,
                              pMort = 0.25,
                              #propDD = 0.25,
                              iCarryCapPupa = NA,
                              fPopNow = NA,
                              fPopPre = NA ) #?not sure whether to provide a default for iCarryCapPupa

{  
  
  #density dependence
  #setting propDD to 0 can stop an density dependence being implemented
#   if ( propDD > 0 )
#     pMort <- rtDensityDependence( fPopn= (sum(vPupaF)+sum(vPupaM)),
#                                   pMort = pMort,
#                                   propDD = propDD,
#                                   iCarryCap = iCarryCapPupa )
    
  #19/6/14 new density dependence mechanism
  #only do it if prev pop > 0 otherwise gives error
  if (fPopPre > 0) {
    slopeDD <- ((fPopNow-fPopPre)/fPopPre)/iCarryCapPupa
    pMortDI <- pMort
    pMort <- pMort + slopeDD * fPopNow
    
    cat("pop pre,now=",round(fPopPre),",",round(fPopNow)," slopeDD=",slopeDD," pMort bef,aft=",pMortDI,",", pMort,"\n")
    
  }
  
  #print(paste0("pop pre,now=",fPopPre,",",fPopNow," slopeDD=",slopeDD," pMort bef,aft=",pMortDI,",", pMort,"\n"))
  #message("pop pre,now=",fPopPre,",",fPopNow," slopeDD=",slopeDD," pMort bef,aft=",pMortDI,",", pMort,"\n")
    
  #OR maybe a simpler & more mechanistic to base on the number of pupae relative to K
   
  
  #pupal mortality is applied once during the pupal period
  #done on day 1
  #vPupaF[1] <- vPupaF[1] * (1-pMort) 
  #vPupaM[1] <- vPupaM[1] * (1-pMort) 
  
  #I couldn't get stability when pupMort was just implemented once in pupal period
  #try to divide the mortality and implement it every day
  #because it is compound I may need to do something special in the division
  #currently pupal period is 28forM & 26forF
  #pMortPerDay <- pMort/length(vPupaF)
  #compound interest calculation
  #to get from yearly to daily
  #r = (fv/pv)^(1/365) -1
  #this is conservative because it divides by F26 so might lead to pop decrease ...
  pMortPerDay <- (1+pMort)^(1/length(vPupaF)) -1  
  
  vPupaF <- vPupaF * (1-pMortPerDay) 
  vPupaM <- vPupaM * (1-pMortPerDay)   
  
  cat("pMortPerDay",pMortPerDay,vPupaF,"\n")
  
  #return the modified vector
  invisible( list(vPupaF=vPupaF,vPupaM=vPupaM) ) 
  
}