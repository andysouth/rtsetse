#' plots map(s) of populations
#' 
#' \code{rtPlotMapPop} plots map(s) of populations from simulation outputs 
#' i.e. a passed array of day,x,y,sex,age.  
#' Can plot a maximum of 16 days, will display the first 16 if you select more than this.
#' For adults to start with but could be used for pupae too. 

#' @param aRecord array of day,x,y,sex,age
#' @param days which days to plot, options 'all', 'final', a number >0 & <days in the simulation, or a series e.g. c(1,2) or c(4:7)
#' @param sex which sex to plot, 'both' or 'MF' for both, 'M' males, 'F' females
#' @param title a title for the plot  
#' 
#' @return ?nothing
#' @examples
#' tst <- rtPhase2Test2()
#' rtPlotMapPop(tst) 
#' @export

rtPlotMapPop <- function( aRecord, 
                          days = 'all',
                          sex = 'MF',
                          title = NULL )
{
  #to sum the age structures in each cell on each day
  #and give result as [days,x,y]
 
  #subsetting of days
  #be careful that day0 is element1
  #Maybe I should drop the day0 thing before it bites me ?? I could set day1 to be the starting conditions. 
  if (days=='all') days <- TRUE
  else if (days=='final') days <- dim(aRecord)[1]
  #else check whether it is numeric and within the bounds of array[1]
  else if ( !is.numeric(days) | days<1 | days>dim(aRecord)[1] ){
    stop("days should be numeric and >0 and <=",dim(aRecord)[1]," or 'all' or 'final', yours is ",days)  
  }
  #else days is just left as it is
  
  #with new output of [day,x,y,sex,age]
  #this does it just for F I could easily do for F+M and allow the option
  #aDays <- apply(aGrid[,,,'F', ,drop=FALSE],MARGIN=c(1,2,3),sum)
  #M+F
  
  sexTitle <- sex
  if (sex=='both' | sex=='MF'){
    sex=TRUE
    sexTitle='M&F'
  } else if (sex != 'M' & sex != 'F') 
    stop("sex should be 'M','F','MF' or 'both', yours is ",sex)
  
  aDays <- apply(aRecord[days,,,sex, ,drop=FALSE],MARGIN=c(1,2,3),sum)  
  
  #I might not need this with the new ,drop=FALSE above
  #if ( input$nRow == 1 || input$nCol == 1 )
  #  dim(aDays) <- dim(aGrid)[-length(dim(aGrid))]
  
  #rearranging dimensions for raster brick
  #needs to be x,y,z
  aDays <- aperm(aDays, c(2, 3, 1))
  brick1 <- brick(aDays) 
  #the day titles for each subplot (otherwise they get lost when subsetted)
  titles <- paste(dimnames(aDays)$day, sexTitle) 
  
  plot(brick1, main=titles)  
  
}