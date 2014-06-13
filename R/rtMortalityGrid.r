#' tsetse mortality on a grid **in development**
#'
#' \code{rtMortalityGrid} returns an array with the age distributions of males & females [x,y,sex,age]
#' \cr It accepts age and sex specific mortality probabilities.
#' \cr !It should check that the length of the age structure and pMort vectors are the same
#' \cr It uses the length of the age structure vectors passed to it.
#' \cr There could be an option to do F only if only F are passed.

#' @param aGrid an array with the age distributions of males & females [x,y,sex,age] 
#' @param vpMortF a vector of age-specific mortality probabilities of Females 
#' @param vpMortM a vector of age-specific mortality probabilities of Males 
#' @param propDD proportion of mortality that is density dependent 
#' @param mCarryCap a matrix of Carrying Capacities for each cell as an integer
#' 
#' @return an array with the age distributions of males & females [x,y,sex,age]
#' @export

rtMortalityGrid <- function( aGrid,
                         vpMortF,
                         vpMortM,
                         propDD = 0.25,
                         mCarryCap = NA ) #?not sure whether to provide a default for iCarryCap
{

  
  ###################
  #density dependence
  #different to pupal & larval mortality in that dd needs to be
  #applied to a vector of age-specific mortality probabilities
  #surprisingly it seems to work on vectors as well as single values
  
#!!! re-implement this next  
#   if ( propDD > 0 )
#   {
#     vpMortF <- rtDensityDependence( fPopn= (sum(vFem)+sum(vMal)),
#                                     pMort = vpMortF,
#                                     propDD = propDD,
#                                     iCarryCap = iCarryCap )
# 
#     vpMortM <- rtDensityDependence( fPopn= (sum(vFem)+sum(vMal)),
#                                     pMort = vpMortM,
#                                     propDD = propDD,
#                                     iCarryCap = iCarryCap )
#   }

  
  
  #how it was done in rtMortality   
#   for( age in seq_along(vFem) )  {
#     vFem[age] <- vFem[age] * (1-vpMortF[age])   
#   }
#   
#   for( age in seq_along(vMal) )  {
#     vMal[age] <- vMal[age] * (1-vpMortM[age])   
#   }
 
  #aGrid[x,y,sex,age]
  #mort differs by sex & age
  #therefore may need to go along the dimension

  #apply would be easier, might be able to combine
  #tst <- apply(aGrid,MARGIN=c('x','y','sex'),identity)

  #first see if I can do for one sex
  #this does output the length of the age vector
  #tst <- apply(aGrid,MARGIN=c('x','y','sex'), function(a) cat(paste(length(a),"\n")))

  #create a low level mort function (similar to what I started with!)
  rtMort <- function(vAges,vpMortByAge){
      #trying as a loop just to get started
      for( age in seq_along(vAges) )  {
        vAges[age] <- vAges[age] * (1-vpMortByAge[age])   
      } 
      #return
      vAges
  }

  #this calls the mortality function for both sexes
  #tst <- apply( aGrid,MARGIN=c('x','y','sex'), rtMort, vpMortByAge=vpMortF )

  #to separate them I might be able to do this
  #F
  aF <- aGrid[,,'F',]
  aF <- apply( aF, MARGIN=c('x','y'), rtMort, vpMortByAge=vpMortF )
  #do I need to aperm ? yes
  aF <- aperm(aF, c(2,3,1))
  #put aF back into grid
  aGrid[,,'F',] <- aF
  #M
  aM <- aGrid[,,'M',]
  aM <- apply( aM, MARGIN=c('x','y'), rtMort, vpMortByAge=vpMortM )
  #do I need to aperm ? yes
  aM <- aperm(aM, c(2,3,1))
  #put aF back into grid
  aGrid[,,'M',] <- aM

  #returning a modified array
  invisible( aGrid )
  
} #end of rtMortality()