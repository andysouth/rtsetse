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
#' @param mMortMultGrid a grid of mortality multipliers (derived from vegetation)
#' @param mCarryCap a matrix of Carrying Capacities for each cell as an integer (an alternative to iCarryCap)
#' @param iCarryCap a single integer Carrying Capacities for all cells as an integer (an alternative to mCarryCap)
#' 
#' @return an array with the age distributions of males & females [x,y,sex,age]
#' @export

rtMortalityGrid <- function( aGrid,
                         vpMortF,
                         vpMortM,
                         propDD = 0.25,
                         mMortMultGrid = NULL,
                         mCarryCap = NULL,
                         iCarryCap = NULL ) 
{
 
  #aGrid[x,y,sex,age]

  #checks
  if (is.null(iCarryCap) & is.null(mCarryCap)) stop("you must specify one of either mCarryCap or iCarryCap")
  
  #check that the mortMult grid matches dimensions of the array
  if ( ! is.null(mMortMultGrid))
  {
    if ( length(dimnames(aGrid)$x) != ncol(mMortMultGrid) | length(dimnames(aGrid)$y) != nrow(mMortMultGrid) )
      stop("xy dimensions of the tsetse grid array",length(dimnames(aGrid)$x),",",length(dimnames(aGrid)$y),
           "don't match those of the mortality multiplier",ncol(mMortMultGrid),",",nrow(mMortMultGrid)) 
  }
    
  for(x in seq_along(dimnames(aGrid)$x)){
    for(y in seq_along(dimnames(aGrid)$y)){
      
      #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
      
      #!BEWARE potentially confusing issue of matrix dimensions
      #!matrices are indexed by rows,cols. rows=y, cols=x
      
      #if no single CarryCap value get it from the grid
      if ( is.null(iCarryCap) ) iCarryCap <- mCarryCap[y,x]  
      
      
      #apply mortality multiplier for this cell
      #BUG1 was here sequential mutliplication of mort rates
      vpMortFVeg <- vpMortF
      vpMortMVeg <- vpMortM
      if ( !is.null(mMortMultGrid) )
      {
        iMortMult <- mMortMultGrid[y,x]
        if (iMortMult != 100)
        {
          #first convert percent to proportion
          iMortMult <- iMortMult/100
          #this is how mortalities are multiplied in hat-trick
          vpMortFVeg <- vpMortF*iMortMult
          vpMortMVeg <- vpMortM*iMortMult 
          #check here that no mortalities go above 1
          if ( max(vpMortFVeg) > 1 | max(vpMortFVeg) > 1 )
            stop("mortality probability has gone above 1")
        }
      } 
      
      
      #only apply mortality if flies in cell (to save time)
      if ( sum(aGrid[x,y,,]) > 0 )
      {
        aGrid[x,y,,] <- rtMortality(vFem = aGrid[x,y,'F',],
                                    vMal = aGrid[x,y,'M',],
                                    vpMortFVeg,
                                    vpMortMVeg,
                                    returnArray = TRUE,
                                    propDD = propDD, 
                                    iCarryCap = iCarryCap )          
      }
      
    }#y
  }#x



  #returning a modified array
  invisible( aGrid )
  
} 