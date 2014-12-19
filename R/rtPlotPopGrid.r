#' plots popn of M&F over multiple days and grid cells
#' 
#' \code{rtPlotPopGrid} plots change in M&F numbers over whole grid 
#' from a passed matrix of day,y,x,sex,age.  
#' Uses a different approach from rtPlotPopAndPupae using reshape2, dplyr & ggplot2.  

#' @param aGrid array of day,y,x,sex,age
#' @param title a title for the plot  
#' 
#' @return ?ggplot2 object
#' @examples
#' tst <- rt_runGridTestSpread()
#' rtPlotPopGrid(tst) 
#' @export

rtPlotPopGrid <- function( aGrid, title=NULL )
{

  #melt from an array to a dataframe using reshape2
  #! note this might be best done before the mat is passed to this function
  melted <- reshape2::melt(aGrid)
  
  #To get day as a number (note -1 to account for day0)
  #melted$dayNum <- as.numeric(melted$day)-1#(-1 was when I started at day0)
  melted$dayNum <- as.numeric(melted$day)
  
  #dplyr
  #sum by day (M&F)
  dfDay <- melted %>%
    group_by(dayNum) %>%
    summarise(total = sum(value) )
  
  #sum by day separate M&F
  dfDaySex <- melted %>%
    group_by(dayNum,sex) %>%
    summarise(total = sum(value) )
  
#   dfDayAge <- melted %>%
#     group_by(dayNum, age) %>%
#     summarise(total = sum(value) 

  theme_set( theme_bw() )            
  
  myplot <- ggplot()
  
  myplot <- myplot + 
    geom_line(data=dfDay, aes(x=dayNum, y=total)) +
    geom_line(data=dfDaySex, aes(x=dayNum, y=total, colour=sex)) +
    labs(title=title)

  print(myplot)
  
}