#' plots feeding results of one hunger cycle for testing
#' 
#' \code{rtPlotFeedingOneCycle} plots feeding results output by \code{\link{rtPhase3Test}}

#' @param dF a dataframe of huntPeriod followed by number of flies hunting or feeding
#' @param title a title for the plot  
#' @param proportionPlot whether to plot just the proportion of flies feeding on humans, FALSE by default  
#' 
#' @return ?ggplot2 object
#' @examples
#' tst <- rtPhase3Test()
#' rtPlotFeedingOneCycle(tst) 
#' @export

rtPlotFeedingOneCycle <- function( dF, 
                                   title="Feeding Test",
                                   proportionPlot=FALSE )
{

if (proportionPlot) {

  #calc proportion of feeders feeding on humans
  dF$manFeedersProportion <- dF$manFeeders/(dF$manFeeders+dF$oxeFeeders)
  #don't need to melt the dF because only one variable to plot
  
  theme_set( theme_bw() )            
  
  myplot <- ggplot()
  
  myplot <- myplot + 
    #miss first point dF[-1,] because it is starting conditions
    geom_line(data=dF[-1,], aes(x=huntPeriod, y=manFeedersProportion)) +
    labs(title=title)
  
  print(myplot)  
  
} else {
  
  #if not proportionPlot
  
  #this melts the dataframe into 3 columns, huntPeriod, variable and value 
  dF2 <- reshape2::melt(dF,id.vars="huntPeriod", variable.name="FlyStatus", value.name="NumFlies")
  
  theme_set( theme_bw() )            
  
  myplot <- ggplot()
  
  myplot <- myplot + 
    geom_line(data=dF2, aes(x=huntPeriod, y=NumFlies, colour=FlyStatus)) +
    labs(title=title)
  
  print(myplot)  
  } #end of if not proportionPlot
  


}

