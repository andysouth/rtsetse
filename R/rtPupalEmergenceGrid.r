#' tsetse pupal emergence on a grid
#'
#' \code{rtPupalEmergenceGrid} moves pupae of both sexes from the oldest pupal age class to the youngest adult age class. 
#' 
#' pupal emergence needs to come from the number of pupae in the ground of the appropriate age 
#' 
#' Initially just gets the number of pupae from the final
#' age class of the passed vector.
#' Sets the number in that final age class to 0 at the same time
#' Would be less code if not in a function ...
#' Want to allow for it being more complex in future ...
#' Can get it to return vectors of both pupae & adults


#' @param aGrid an array with the age distributions of males & females [x,y,sex,age] 
#' @param aGridPup an array with the age distributions of pupal males & females [x,y,sex,age] 
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(M) to develop
#' 
#' @return a list containing aGrid, aGridPup
#' @export

rtPupalEmergenceGrid <- function( aGrid,                                
                                  aGridPup,
                                  iPupDurF = 26,
                                  iPupDurM = 28 )
{
  
  #this follows hat-trick in that all pupae in final age class emerge
  #it could be made more probabilistic
  
#   #this is how it was done in rtPupalEmergence
#   #set 1st age class of adults from last age class of pupae
#   vPopF[1] <- vPupaF[length(vPupaF)]
#   vPopM[1] <- vPupaM[length(vPupaM)]  
#   #set last age class of pupae to 0
#   vPupaF[length(vPupaF)] <- 0
#   vPupaM[length(vPupaM)] <- 0

    #set 1st age class of adults from last age class of pupae
    #   aGrid[,,'M','age2'] #a grid of one age    
    aGrid[,,'F','age1'] <- aGridPup[,,'F',iPupDurF]
    aGrid[,,'M','age1'] <- aGridPup[,,'M',iPupDurM]
    #set last age class of pupae to 0
    aGridPup[,,'F',iPupDurF] <- 0
    aGridPup[,,'M',iPupDurM] <- 0  
  
  
  #return the modified arrays
  invisible( list(aGrid=aGrid, aGridPup=aGridPup) )
  
}