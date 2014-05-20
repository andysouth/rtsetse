#' plots mean age of a popn over time
#' 
#' \code{rtPlotMeanAge} plots change in mean age from a single passed dataframe 

#' @param df a dataframe of number of individuals with days in columns and ages in rows
#' @param title a title for the plot  
#' 
#' @return nothing
#' @examples
#' tst <- rtPhase1Test(iDays=100)
# 'rtPlotMeanAge(tst$dfRecordF + tst$dfRecordM)

#' @export

rtPlotMeanAge <- function( df, title=NULL )
{
  
  #!! this is an ugly way of doing & i should change
  
  #calculate cumulative age in each age class on each day
  #the rev(seq) bit gives the ages of each age class
  dfCumAge <- df*rev(seq(df[,1]))
  
  dfPop <- data.frame( lapply(df,sum) )
  dfTot <- data.frame( lapply(dfCumAge,sum) )
  
  dfMeanAge <- dfTot / dfPop

  #then do I just divide it by total number of individuals to get the mean age ?
  
  
  #transpose to make plotting easier
  vMeanAge <- t(dfMeanAge)
  
  plot(vMeanAge, type='l', xlab='day', ylab='mean age', col="red", main=title)
  #lines(vTotPupa,col='blue')
  #legend("top", legend=c("adults","pupae"), col=c("red","blue"), lty=1, horiz=TRUE)
  
}
#to test
#rtPlotPopAndPupae(tst$dfRecordF, tst$dfRecordM, tst$dfRecordPupaF, tst$dfRecordPupaM)
