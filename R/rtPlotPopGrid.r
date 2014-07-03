#' plots popn of M&F over multiple days and grid cells
#' 
#' \code{rtPlotPopGrid} plots change in M&F numbers over whole grid 
#' from a passed matrix of day,x,y,sex,age.  
#' Uses a different approach from rtPlotPopAndPupae using reshape2, dplyr & ggplot2.  
#' EXPERIMENTAL

#' @param mat matrix of day,x,y,sex,age
#' @param title a title for the plot  
#' 
#' @return ?ggplot2 object
#' @examples
#' tst <- rtPhase2Test2()
#' rtPlotPopGrid(tst) 
#' @export

rtPlotPopGrid <- function( mat, title=NULL )
{

  #melt from an array to a dataframe using reshape2
  #! note this might be best done before the mat is passed to this function
  melted <- melt(mat)
  
  #To get day as a number (note -1 to account for day0)
  melted$dayNum <- as.numeric(melted$day)-1
  
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
              
  
  myplot <- ggplot()
  
  myplot + 
    geom_line(data=dfDay, aes(x=dayNum, y=total)) +
    geom_line(data=dfDaySex, aes(x=dayNum, y=total, colour=sex)) +
    labs(title=title)
  
}