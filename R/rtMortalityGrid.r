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
                         mCarryCap = NA,
                         loop = TRUE ) #?not sure whether to provide a default for iCarryCap
{
 
 
  #aGrid[x,y,sex,age]
  #mort differs by sex & age
  #therefore may need to go along the dimension

  #first see if I can do for one sex
  #this does output the length of the age vector
  #tst <- apply(aGrid,MARGIN=c('x','y','sex'), function(a) cat(paste(length(a),"\n")))

  if (!loop){

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

  #to apply mort to sexes separately (for each cell in the grid)
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

  } else {
    
    #LOOP solution
    
    #to implement DD on a grid 
    #it may be best/simples to use a loop
    #then I can call the existing rtMortality() function
    #EXCEPT I'll need to modify how it returns from a list to an array
    for(x in seq_along(dimnames(aGrid)$x)){
      for(y in seq_along(dimnames(aGrid)$y)){
        
        #get carry cap from the matrix
        iCarryCap <- mCarryCap[x,y]   
        #iCarryCap <- 200

# this is done in rtMortality() so not needed here        
#         if ( propDD > 0 )
#           {
#             #total pop in the cell
#             fPopn <- sum(aGrid[x,y,,])
#             vpMortF <- rtDensityDependence( fPopn = fPopn,
#                                             pMort = vpMortF,
#                                             propDD = propDD,
#                                             iCarryCap = iCarryCap )
#         
#             vpMortM <- rtDensityDependence( fPopn = fPopn,
#                                             pMort = vpMortM,
#                                             propDD = propDD,
#                                             iCarryCap = iCarryCap )
#           }
        
        
        aGrid[x,y,,] <- rtMortality(vFem = aGrid[x,y,'F',],
                                    vMal = aGrid[x,y,'M',],
                                    vpMortF,
                                    vpMortM,
                                    returnArray = TRUE,
                                    propDD = propDD, 
                                    iCarryCap = iCarryCap )
        }#y
    }#x
  }#end of loop solution




  #returning a modified array
  invisible( aGrid )
  
} #end of rtMortality()