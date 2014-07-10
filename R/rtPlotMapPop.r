#' plots map(s) of populations
#' 
#' \code{rtPlotMapPop} plots maps of populations from simulation outputs 
#' i.e. a passed array of day,x,y,sex,age.  
#' For adults to start with but could be used for pupae too. 

#' @param aRecord array of day,x,y,sex,age
#' @param days which days to plot, options 'all', a number, c(1,2), 'final'
#' @param title a title for the plot  
#' 
#' @return ?nothing
#' @examples
#' tst <- rtPhase2Test2()
#' rtPlotMapPop(tst) 
#' @export

rtPlotMapPop <- function( aRecord, 
                          days = 'all',
                          title=NULL )
{
  #to sum the age structures in each cell on each day
  #and give result as [days,x,y]
 
  if (days=='all') cat('here')
  
  #with new output of [day,x,y,sex,age]
  #this does it just for F I could easily do for F+M and allow the option
  #aDays <- apply(aGrid[,,,'F', ,drop=FALSE],MARGIN=c(1,2,3),sum)
  #M+F
  aDays <- apply(aRecord[,,,, ,drop=FALSE],MARGIN=c(1,2,3),sum)  
  
  #I might not need this with the new ,drop=FALSE above
  #if ( input$nRow == 1 || input$nCol == 1 )
  #  dim(aDays) <- dim(aGrid)[-length(dim(aGrid))]
  
  #rearranging dimensions for raster brick
  #needs to be x,y,z
  aDays <- aperm(aDays, c(2, 3, 1))
  brick1 <- brick(aDays) 
  plot(brick1)  
  
}