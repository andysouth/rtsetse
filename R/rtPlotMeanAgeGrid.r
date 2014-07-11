#' plots mean age of M&F over multiple days and grid cells
#' 
#' \code{rtPlotMeanAgeGrid} plots change in mean age of M&F over whole grid 
#' from a passed matrix of day,x,y,sex,age.  
#' Uses a different approach from rtPlotPopAndPupae using reshape2, dplyr & ggplot2.  
#' EXPERIMENTAL

#' @param aGrid array of day,x,y,sex,age
#' @param title a title for the plot  
#' 
#' @return ?ggplot2 object
#' @examples
#' tst <- rtPhase2Test2()
#' rtPlotMeanAgeGrid(tst) 
#' @export

rtPlotMeanAgeGrid <- function( aGrid, title="Mean Age across whole grid" )
{
  
  #melt from an array to a dataframe using reshape2
  #! note this might be best done before the mat is passed to this function
  melted <- reshape2::melt(aGrid)
  
  #To get day as a number 
  #melted$dayNum <- as.numeric(melted$day)-1 #(-1 was when I started at day0)
  melted$dayNum <- as.numeric(melted$day)
  melted$ageNum <- as.numeric(melted$age)  

  #dplyr
  #i want to calc the cumulative ages by day for M&F
  #this might work, but currently they are all age1 so difficult to tell !
  dfCumAgeDaySex <- melted %>%
    group_by(dayNum,sex) %>%
    summarise(total=sum(value), cumAge = sum(value*ageNum) )  
   
  dfCumAgeDaySex$meanAge <- dfCumAgeDaySex$cumAge / dfCumAgeDaySex$total
  
  theme_set( theme_bw() )            
  
  myplot <- ggplot()
  
  myplot <- myplot + 
    geom_line(data=dfCumAgeDaySex, aes(x=dayNum, y=meanAge, colour=sex)) +
    labs(title=title)
  
  print(myplot)
  
}