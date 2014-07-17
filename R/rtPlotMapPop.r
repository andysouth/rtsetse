#' plots map(s) of populations
#' 
#' \code{rtPlotMapPop} plots map(s) of populations from simulation outputs 
#' i.e. a passed array of day,x,y,sex,age.  
#' Can plot a maximum of 16 days, will display the first 16 if you select more than this.
#' For adults to start with but could be used for pupae too. 

#' @param aRecord array of day,x,y,sex,age
#' @param days which days to plot, options 'all', 'final', 
#'    a number >0 and <days in the simulation, or a series e.g. c(1,2) or c(4:7)
#' @param ifManyDays days to plot if days='all' and num days > 16, Options: 
#'    'first' first 16 days, 'last' 16 days, 'firstlast' first8 and last8, 
#'    'spread' try to spread out days, e.g. if 32 it would plot 2,4,6, etc. can lead to uneven intervals.
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
                          ifManyDays = 'spread',
                          sex = 'MF',
                          title = NULL )
{
  #to sum the age structures in each cell on each day
  #and give result as [days,x,y]
 
  #subsetting of days
  #be careful that day0 is element1
  #Maybe I should drop the day0 thing before it bites me ?? I could set day1 to be the starting conditions. 
  numDays <- dim(aRecord)[1]
  if (days=='all') days <- TRUE
  else if (days=='final') days <- numDays
  #else check whether it is numeric and within the bounds of array[1]
  else if ( !is.numeric(days) | days<1 | days>numDays ){
    stop("days should be numeric and >0 and <=",numDays," or 'all' or 'final', yours is ",days)  
  }
  #else days is just left as it is
  
  #what to do if days=='all' and sim longer than 16 days
  if( days==TRUE & numDays>16 ) {
    if (ifManyDays=='last') days <- (numDays-15):numDays
    else if (ifManyDays=='first') days <- 1:16
    else if (ifManyDays=='firstlast') days <- c(1:8, (numDays-7):numDays)
    else if (ifManyDays=='spread') days <- seq(from=1, to=numDays, by=numDays/16)
    else stop("ifManyDays should be 'last','first' or 'spread' yours is ",ifManyDays)
  }
  
  
  sexTitle <- sex
  if (sex=='both' | sex=='MF'){
    sex=TRUE
    sexTitle='M&F'
  } else if (sex != 'M' & sex != 'F') 
    stop("sex should be 'M','F','MF' or 'both', yours is ",sex)
  
  #this gives an array of [days,x,y]
  aDays <- apply(aRecord[days,,,sex, ,drop=FALSE],MARGIN=c(1,2,3),sum)  
  
  #I might not need this with the new ,drop=FALSE above
  #if ( input$nRow == 1 || input$nCol == 1 )
  #  dim(aDays) <- dim(aGrid)[-length(dim(aGrid))]
  
  #set the color scale constant for all plots between 0 & max cell val
  #find the maximum cell value on any day
  fMaxCellVal <- max( aDays )
  #breaks <- seq(0, fMaxCellVal, length.out=10)
  breaks <- pretty(c(0,fMaxCellVal), n=6) #n is how many intervals
  #can I add on a low cat of 0-1 flies
  #it does make the legend look a bit weird coz it overplots 0&1
  #but does make results clearer to me in test phase at least
  breaks <- c(0,1,breaks[-1])
  #to avoid error of 'breaks' are not unique
  breaks <- unique(breaks)  
  
  nb <- length(breaks)-1 
  colourP <- rev(terrain.colors(nb)) 
  #colourP <- topo.colors(nb) 
  
  #rearranging dimensions for raster brick
  #needs to be x,y,z
  aDays <- aperm(aDays, c(2, 3, 1))
  brick1 <- brick(aDays) 
  #the day titles for each subplot (otherwise they get lost when subsetted)
  titles <- paste(dimnames(aDays)$day, sexTitle) 
  
  plot(brick1, main=titles, breaks=breaks, col=colourP)  
  
}