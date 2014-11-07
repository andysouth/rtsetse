#' tsetse pupal mortality on a GRID
#'
#' \code{rtPupalMortalityGrid} applies \code{\link{rtPupalMortality}} to a grid.
#' It does that by simply calling that function for each cell in the grid.
#' 
#' @param aGridPup array with the age distributions of pupal males & females [x,y,sex,age]  
#' @param pMort a mortality probability 
#' @param propDD proportion of mortality that is density dependent 
#' @param mCarryCap matrix [x,y] of carrying capacities
#' 
#' @return updated aGridPup
#' @export

rtPupalMortalityGrid <- function( aGridPup,
                                  pMort = 0.25,
                                  propDD = 0.25,
                                  mCarryCap = NA ) 
  
{  
  
  #LOOP solution
  #to implement DD on a grid 
  
  #calls existing rtMortality() function (modified to return an array)
  
  for(x in seq_along(dimnames(aGridPup)$x)){
    for(y in seq_along(dimnames(aGridPup)$y)){
      
      #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
      
      #!BEWARE potentially confusing issue of matrix dimensions
      #!matrices are indexed by rows,cols. rows=y, cols=x
      iCarryCap <- mCarryCap[y,x] 
      
      
      vPupaF <- aGridPup[x,y,'F',] #an age structure for one cell
      vPupaM <- aGridPup[x,y,'M',]  
      
      #these 3 lines are the same way it is called in rtPhase1Test3()
      #!BEWARE iCarryCapPupa set from iCarryCap here because hat-trick default runs show similar numbers of ads & pupae at stability
      lPupae <- rtPupalMortality(vPupaF=vPupaF, 
                                 vPupaM=vPupaM, 
                                 pMort=pMort, 
                                 propDD=propDD, 
                                 iCarryCapPupa=iCarryCap )
      
      vPupaF <- lPupae$vPupaF
      vPupaM <- lPupae$vPupaM
      
      aGridPup[x,y,'F',] <- vPupaF
      aGridPup[x,y,'M',] <- vPupaM      
      
    }#y
  }#x
  
  
  
  #return the modified array
  invisible( aGridPup ) 
  
}

