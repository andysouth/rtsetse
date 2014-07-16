#' tsetse pupal mortality on a GRID following Rogers(1990)
#'
#' \code{rtPupalMortalityRogersGrid} applies \code{\link{rtPupalMortalityRogers}} to a grid.
#' It does that by simply calling that function for each cell in the grid.
#' 
#' @param aGridPup array with the age distributions of pupal males & females [x,y,sex,age]  
#' @param pMortPupa a density independent mortality probability 
#' @param iPupaDensThresh the threshold above which density dependence acts
#' @param fSlopeDD the slope of density dependence, how mortality increases with density
#' 
#' @return updated aGridPup
#' @export

rtPupalMortalityRogersGrid <- function( aGridPup,
                                    pMortPupa = 0.25,
                                    iPupaDensThresh = 200,
                                    fSlopeDD = 1.0 ) 
  
{  
  
  #LOOP solution
  #to implement DD on a grid 
  
  #calls existing rtMortality() function (modified to return an array)
  
  for(x in seq_along(dimnames(aGridPup)$x)){
    for(y in seq_along(dimnames(aGridPup)$y)){
      
      #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
      
      #!!!annoying issue about confusing dimensions
      #!!!i changed int around just to get it to work    
      #get carry cap from the matrix
      #iCarryCap <- mCarryCap[x,y]  
      #iCarryCap <- mCarryCap[y,x] 
      #!! this takes pupal density threshold from carryCap
      #!! i'm still unsure how we might choose to implement this
      #!! and want to retain flexibility for now
      #actually matrix of cc's not yet setup as an input
      #iPupaDensThresh <- mCarryCap[y,x]
      
      vPupaF <- aGridPup[x,y,'F',] #an age structure for one cell
      vPupaM <- aGridPup[x,y,'M',]  
      
      #these 3 lines are the same way it is called in rtPhase1Test2()
      #I could improve it here and there
      lPupae <- rtPupalMortalityRogers(vPupaF=vPupaF, 
                                       vPupaM=vPupaM, 
                                       pMort=pMortPupa, 
                                       iPupaDensThresh=iPupaDensThresh, 
                                       fSlopeDD=fSlopeDD )     
      vPupaF <- lPupae$vPupaF
      vPupaM <- lPupae$vPupaM
      
      aGridPup[x,y,'F',] <- vPupaF
      aGridPup[x,y,'M',] <- vPupaM      
      
    }#y
  }#x
  
  
  
  #return the modified array
  invisible( aGridPup ) 
  
}

