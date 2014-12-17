#' tsetse pupal mortality on a GRID
#'
#' \code{rtPupalMortalityGrid} applies \code{\link{rtPupalMortality}} to a grid.
#' It does that by simply calling that function for each cell in the grid.
#' 
#' @param aGridPup array with the age distributions of pupal males & females [y,x,sex,age]  
#' @param pMort a mortality probability 
#' @param propDD proportion of mortality that is density dependent 
#' @param mMortMultGridPup an optional grid of mortality multipliers (derived from vegetation)
#' @param mCarryCap a matrix of Carrying Capacities for each cell as an integer (an alternative to iCarryCap)
#' @param iCarryCap a single integer Carrying Capacities for all cells as an integer (an alternative to mCarryCap)

#' 
#' @return updated aGridPup
#' @export

rtPupalMortalityGrid <- function( aGridPup,
                                  pMort = 0.25,
                                  propDD = 0.25,
                                  mMortMultGridPup = NULL,
                                  mCarryCap = NULL,
                                  iCarryCap = NULL ) 
  
{  
  
  #checks
  if (is.null(iCarryCap) & is.null(mCarryCap)) stop("you must specify one of either mCarryCap or iCarryCap")
  
  #check that the mortMult grid matches dimensions of the array
  if ( ! is.null(mMortMultGridPup))
  {
    if ( length(dimnames(aGrid)$y) != dim(mMortMultGridPup)[1] | length(dimnames(aGrid)$x) != dim(mMortMultGridPup)[2] )
      stop("yx dimensions of the tsetse grid array",length(dimnames(aGrid)$y),",",length(dimnames(aGrid)$x),
           "don't match those of the mortality multiplier",dim(mMortMultGridPup)[1],",",dim(mMortMultGridPup)[2]) 
  }
  
  iHighMortCounter <- 0 #to count if mort goes above 1
  
  for(x in seq_along(dimnames(aGridPup)$x)){
    for(y in seq_along(dimnames(aGridPup)$y)){
      
      #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
      
      #!BEWARE potentially confusing issue of matrix dimensions
      #!matrices are indexed by rows,cols. rows=y, cols=x
      
      #if no single CarryCap value get it from the grid - not a standard feature
      if ( is.null(iCarryCap) ) iCarryCap <- mCarryCap[y,x]  
      
      vPupaF <- aGridPup[y,x,'F',] #an age structure for one cell
      vPupaM <- aGridPup[y,x,'M',]  
      
      if ( !is.null(mMortMultGridPup) )
      {
        iMortMult <- mMortMultGridPup[y,x]
        if (iMortMult != 100)
        {
          #first convert percent to proportion
          iMortMult <- iMortMult/100
          #this is how mortalities are multiplied in hat-trick
          pMort <- pMort*iMortMult
          #check here that no mortalities go above 1
          if ( pMort > 1 )
          {
            iHighMortCounter <- iHighMortCounter + 1
            pMort <- 1
          }
        }
      } 
      
      
      
      #these 3 lines are the same way it is called in rtPhase1Test3()
      #!BEWARE iCarryCapPupa set from iCarryCap here because hat-trick default runs show similar numbers of ads & pupae at stability
      lPupae <- rtPupalMortality(vPupaF=vPupaF, 
                                 vPupaM=vPupaM, 
                                 pMort=pMort, 
                                 propDD=propDD,
                                 iCarryCapPupa=iCarryCap )
      
      vPupaF <- lPupae$vPupaF
      vPupaM <- lPupae$vPupaM
      
      aGridPup[y,x,'F',] <- vPupaF
      aGridPup[y,x,'M',] <- vPupaM      
      
    }#y
  }#x
  
  if ( iHighMortCounter > 0  )
    warning("pupal mortality probabilities went above 1 and were set to 1", iHighMortCounter,"times")  
  
  
  #return the modified array
  invisible( aGridPup ) 
  
}

