#' tsetse feeding on a grid **in development**
#'
#' \code{rtFeedingGrid} will work through the gridded tsetse population by grid cell, sex and age
#' \cr and work out numbers feeding on himans and numbers starving.
#' \cr !! it will need to accept grids with the distribution of humans and non-humans

#' @param aGrid an array with the age distributions of males & females [x,y,sex,age] 
#' @param pDetect TEMPORARY starting arg, single probability of detection
#' @param pFeed TEMPORARY starting arg, single probability of feeding once detected 
#' 
#' @return a list of 2 arrays aGridManFeeders and aGridStarved each with the dimensions [x,y,sex,age]
#' @examples
#' \dontrun{
#' tst <- rtPhase2Test2()
#' aGrid <- rtGetFromRecord(day=2)
#' fed <- rtFeedingGrid(aGrid)
#' rtGetFromGrid(fed, sex='sum', age='sum')
#' }
#' @export

rtFeedingGrid <- function( aGrid,
                             pDetect=0.1,
                             pFeed=0.1 ) 
{
   
  #aGrid[x,y,sex,age]
  #create empty grids to hold manFeeders and starved 
  #by copying from aGrid & filling with zeroes
  aGridStarved <- aGrid
  aGridStarved[,,,] <- 0
  aGridManFeeders <- aGridStarved
  
  #it may be possible to replace this with a non-loop apply-type solution later
    
  for(x in seq_along(dimnames(aGrid)$x)){
    for(y in seq_along(dimnames(aGrid)$y)){
      for(sex in seq_along(dimnames(aGrid)$sex)){
        for(age in seq_along(dimnames(aGrid)$age)){
          
          cat(paste("x,y:",x,",",y,"sex:",sex,"age:",age,"\n"))
          
          #is this age the last day of a hunger cycle
          #hunger cycles end on day 1 & every 3 days after that
          if ( age==1 | (age-1)%%3==0 ) {
            
            #15 hunt periods on first day, 30 thereafter
            iNumHuntPeriods <- ifelse( age==1, yes=15, no=30 )
            
            #get the number of hunters in this cell of this sex and age
            hunters <- rtGetFromGrid( aGrid, x=x, y=y, sex=sex, age=age )
            
            #!!for first implementation
            #!!assume 1 person per cell
            #!!and that flies only feed on men
            
            for( iHuntPeriod in 1:iNumHuntPeriods ) {
              
              #detection
#               nonDetectors = hunters * probabilityX
#               manDetectors = hunters * probabilityY
#               oxeDetectors = hunters * probabilityZ
              manDetectors <- hunters * pDetect
              
              #feeding
              manFeeders <- manDetectors * pFeed
              oxeFeeders <- 0 # oxeDetectors * probabilityB
              nonFeeders <- hunters - (manFeeders + oxeFeeders) 
              
              #to go into the next period
              hunters <- nonFeeders
              
              #accumulate manFeeders for the day
              aGridManFeeders[x,y,sex,age] <- aGridManFeeders[x,y,sex,age] + manFeeders
              
            } #end of hunt periods
            
            #set starved to the remaining hunters at the end of hunt periods
            aGridStarved[x,y,sex,age] <- hunters
            
          } #end of if this is a hunger cycle        
        }#age        
      }#sex
    }#y
  }#x
    
  
  
  
  #returning a list of aGridStarved & aGridManFeeders
  #! as initial test just return manFeeders
  invisible( aGridManFeeders )
  
} #end of rtMortality()