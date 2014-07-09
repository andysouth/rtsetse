#' tsetse larval deposition on a grid
#'
#' \code{rtLarvalDepositionGrid} returns the number of male and female larvae deposited in each grid cell
#' 
#' from the age structure of adult females
#' and 'birth' probabilities per female.  


#' @param aGrid array with the age distributions of males & females [x,y,sex,age] 
#' @param aGridPup array with the age distributions of pupal males & females [x,y,sex,age]  
#' @param vpDeposit a vector of age-specific deposition probabilities of Females 
#' 
#' @return a list containing integer numbers of larvae: iLarvaeF, iLarvaeM
#' @export

rtLarvalDepositionGrid <- function( aGrid,
                                    aGridPup,
                                    vpDeposit )
{
  
  #multiply females in each age class by proportion depositing a larva
  #larval mortality is already accounted for in the deposition rates.
  #Removed the rounding to an integer which was used in rtLarvalDeposition 
  
  #to do on a grid
  #i want to multiply the F age structure in each cell by the age dependent deposition rates
  
  #aGrid['x1','y1','F',] #F age structure for one cell
  #drop=FALSE stops array dimensions being lost e.g. if x,y == 1
  aFs <- aGrid[,,'F', ,drop=FALSE] #F age structures for all cells
    
  #create a test vpDeposit
  #vpDeposit <- seq(length(aFs[1,1,]))
  
  #line that multiplies each age structure by age specific probability vector
  tst2 <- apply(aFs,MARGIN=c('x','y'), function(v) v*vpDeposit )    
  #to put array components back in correct order
  aFs2 <- aperm(tst2, c(2,3,1))
  
  #the above seems to work
  #in the test it multiplied age 2 by 2 as expected from the vector1,2 etc.
  
  #Now want to sum the larvae per cell
  #and it might be returned as a matrix [x,y]
  mLarvae <- apply(aFs2, MARGIN=c('x','y'), sum) 
  
  #testing total larvae
  print(mLarvae)  
  
  #assume that half larvae are male and half are female
  #keeping this very simple & transparent for now
  #to avoid confusing myself
  mLarvaeF <- mLarvae / 2
  mLarvaeM <- mLarvae / 2
  
  #now I want to copy these to pupae age1 of each sex
  aGridPup[,,'F','age1'] <- mLarvaeF
  aGridPup[,,'M','age1'] <- mLarvaeM
  
  #it would be easy to make the model probabilistic by assigning gender with a 0.5 probability
  
  #return
  #invisible( list(iLarvaeF=iLarvaeF, iLarvaeM=iLarvaeM) )
  invisible( aGridPup )
  
} 