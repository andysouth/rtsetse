#' tsetse feeding on a grid 
#' 
#' \code{rtFeedingGrid} will work through the gridded tsetse population by grid cell, sex and age
#' \cr and work out numbers feeding on himans and numbers starving.
#' \cr !! it will need to accept grids with the distribution of humans and non-humans

#' @param aGrid an array with the age distributions of males & females [x,y,sex,age] 
#' @param pDetectMan probability of detecting one human per km2
#' @param pDetectOxe probability of detecting one other host (ox equivalent) per km2
#' @param pFeedMan probability of feeding on a human once detected 
#' @param pFeedOxe probability of feeding on other host (ox equivalent) once detected 
#' @param fDensityMan density of humans per km2 in tsetse habitat 
#' @param fDensityOxe density of other hosts (ox equivalents) per km2 in tsetse habitat 
#' @param testing whether to output testing messages to the console 
#' 
#' @return a list of 2 arrays aGridManFeeders and aGridStarved each with the dimensions [x,y,sex,age]
#' @examples
#' \dontrun{
#' tst <- rtPhase2Test2(nRow=3,nCol=3,iMaxAge=7)
#' aGrid <- rtGetFromRecord(tst, day=2)
#' fed <- rtFeedingGrid(aGrid)
#' rtGetFromGrid(fed, sex='sum', age='sum')
#' 
#' #I would expect that this should give similar result to aGrid
#' #when the probabilities are set to 1
#' fed <- rtFeedingGrid(aGrid, pDetect=1, pFeed=1)
#' #not necessarily
#' #AHA! That's OK it's because only those flies of an age that 
#' #they are on the last day of a feeding cycle will feed.
#' }
#' @export

rtFeedingGrid <- function( aGrid,
                           pDetectMan=0.001,
                           pDetectOxe=0.005,
                           pFeedMan=0.1,
                           pFeedOxe=0.8,
                           fDensityMan=1,
                           fDensityOxe=10,
                           testing = TRUE ) 
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
          
          #cat(paste("x,y:",x,",",y,"sex:",sex,"age:",age,"\n"))
          
          #is this age the last day of a hunger cycle
          #hunger cycles end on day 1 & every 3 days after that
          if ( age==1 | (age-1)%%3==0 ) {
            
            #15 hunt periods on first day, 30 thereafter
            iNumHuntPeriods <- ifelse( age==1, yes=15, no=30 )
            
            #get the number of hunters in this cell of this sex and age
            fHunters <- rtGetFromGrid( aGrid, x=x, y=y, sex=sex, age=age )
            
            #!!for first implementation
            #!!assume 1 person per cell
            #!!and that flies only feed on men
            
            for( iHuntPeriod in 1:iNumHuntPeriods ) {
              
              #call function for one hunt period
              lF <- rtFeedingOneHuntPeriod(fHunters=fHunters, 
                                           pDetectMan = pDetectMan,
                                           pDetectOxe = pDetectOxe,
                                           pFeedMan = pFeedMan,
                                           pFeedOxe = pFeedOxe,
                                           fDensityMan = fDensityMan,
                                           fDensityOxe = fDensityOxe,                                           
                                           testing = testing )
              
              #to go into the next period
              fHunters <- lF$fHunters
              
              #accumulate manFeeders for the day
              aGridManFeeders[x,y,sex,age] <- aGridManFeeders[x,y,sex,age] + lF$fManFeeders
              
            } #end of hunt periods
            
            #set starved to the remaining hunters at the end of hunt periods
            aGridStarved[x,y,sex,age] <- fHunters
            
            #outputs for testing
            if (testing) { 
              testFeeders <- signif(aGridManFeeders[x,y,sex,age],digits=2)
              testPropFeeders <- signif((aGridManFeeders[x,y,sex,age]/aGrid[x,y,sex,age]),digits=2)
              cat(paste("feeding x,y:",x,",",y,"sex:",sex,"age:",age,"manFeeders:",testFeeders,"proportion:",testPropFeeders,"\n"))
            } #end of testing

          } #end of if this is a hunger cycle        
        }#age        
      }#sex
    }#y
  }#x
    
  
  #returning a list of aGridStarved & aGridManFeeders
  #invisible( list(aGridManFeeders=aGridManFeeders, aGridStarved=aGridStarved) )

  #! as initial test just return manFeeders
  invisible( aGridManFeeders )


  
} 