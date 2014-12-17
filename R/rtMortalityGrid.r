#' tsetse mortality on a grid **in development**
#'
#' \code{rtMortalityGrid} returns an array with the age distributions of males & females [x,y,sex,age]
#' \cr It accepts age and sex specific mortality probabilities.
#' \cr !It should check that the length of the age structure and pMort vectors are the same
#' \cr It uses the length of the age structure vectors passed to it.
#' \cr There could be an option to do F only if only F are passed.

#' @param aGrid an array with the age distributions of males & females [y,x,sex,age] 
#' @param vpMortF a vector of age-specific mortality probabilities of Females 
#' @param vpMortM a vector of age-specific mortality probabilities of Males 
#' @param propDD proportion of mortality that is density dependent 
#' @param mMortMultGrid an optional grid of mortality multipliers (derived from vegetation)
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
    if ( length(dimnames(aGrid)$y) != dim(mMortMultGrid)[1] | length(dimnames(aGrid)$x) != dim(mMortMultGrid)[2] )
      stop("yx dimensions of the tsetse grid array",length(dimnames(aGrid)$y),",",length(dimnames(aGrid)$x),
           "don't match those of the mortality multiplier",dim(mMortMultGrid)[1],",",dim(mMortMultGrid)[2]) 
  }
    
  iHighMortCounter <- 0 #to count if mort goes above 1
  for(x in seq_along(dimnames(aGrid)$x)){
    for(y in seq_along(dimnames(aGrid)$y)){
      
      #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
      
      #only apply mortality if flies in cell (to save time)
      if ( sum(aGrid[y,x,,]) > 0 )
      {
      
        #!BEWARE potentially confusing issue of matrix dimensions
        #!matrices are indexed by rows,cols. rows=y, cols=x
        #!but in rtsetse all matrices now referenced [x,y]
        
        #if no single CarryCap value get it from the grid (not a standard feature)
        if ( is.null(iCarryCap) ) iCarryCap <- mCarryCap[y,x]     
        
        #apply mortality multiplier for this cell
        #TODO this doesn't seem like an efficient way of doing
        #should I instead create an array of the age-specific mortalities
        #but might get tricky
        vpMortFVeg <- vpMortF
        vpMortMVeg <- vpMortM
        if ( !is.null(mMortMultGrid) )
        {
          #iMortMult <- mMortMultGrid[y,x]
          #in rtsetse all matrices now referenced [x,y]
          iMortMult <- mMortMultGrid[y,x]
          if (iMortMult != 100)
          {
            #first convert percent to proportion
            iMortMult <- iMortMult/100
            #this is how mortalities are multiplied in hat-trick
            vpMortFVeg <- vpMortF*iMortMult
            vpMortMVeg <- vpMortM*iMortMult 
            #check here that no mortalities go above 1
            if ( max(vpMortFVeg) > 1 | max(vpMortMVeg) > 1 )
            {
              iHighMortCounter <- iHighMortCounter + 1
              vpMortFVeg[ which(vpMortFVeg>1) ] <- 1
              vpMortMVeg[ which(vpMortMVeg>1) ] <- 1
            }
          }
        } 
      
        #apply mortality for this cell
        aGrid[y,x,,] <- rtMortality(vFem = aGrid[y,x,'F',],
                                    vMal = aGrid[y,x,'M',],
                                    vpMortFVeg,
                                    vpMortMVeg,
                                    returnArray = TRUE,
                                    propDD = propDD, 
                                    iCarryCap = iCarryCap )  
        
      }#end of if pop in cell > 0     
    }#y
  }#x

  if ( iHighMortCounter > 0  )
    warning("mortality probabilities went above 1 and were set to 1", iHighMortCounter,"times")  

  #returning a modified array
  invisible( aGrid )
  
} 