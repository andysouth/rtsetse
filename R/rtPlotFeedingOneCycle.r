#' plots feeding results of one hunger cycle for testing
#' 
#' \code{rtPlotFeedingOneCycle} plots feeding results output by \code{\link{rtPhase3Test}}

#' @param dF a dataframe of huntPeriod followed by number of flies hunting or feeding
#' @param title a title for the plot  
#' 
#' @return ?ggplot2 object
#' @examples
#' tst <- rtPhase3Test()
#' rtPlotFeedingOneCycle(tst) 
#' @export

rtPlotFeedingOneCycle <- function( dF, title="Feeding Test" )
{

#this melts the dataframe into 3 columns, huntPeriod, variable and value 
dF2 <- reshape2::melt(dF,id.vars="huntPeriod", variable.name="FlyStatus", value.name="NumFlies")


theme_set( theme_bw() )            

myplot <- ggplot()

myplot <- myplot + 
  geom_line(data=dF2, aes(x=huntPeriod, y=NumFlies, colour=FlyStatus)) +
  labs(title=title)

print(myplot)

}

