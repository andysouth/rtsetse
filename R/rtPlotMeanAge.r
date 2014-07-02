#' plots mean age of a popn over time
#' 
#' \code{rtPlotMeanAge} plots change in mean age passed dataframes for males and females. 
#' Will also work on just a single dataframe.

#' @param dfF a dataframe of number of females with days in columns and ages in rows
#' @param dfM a dataframe of number of males with days in columns and ages in rows
#' @param title a title for the plot  
#' 
#' @return nothing
#' @examples
#' tst <- rtPhase1Test(iDays=100)
# 'rtPlotMeanAge(tst$dfRecordF, tst$dfRecordM)

#' @export

rtPlotMeanAge <- function( dfF, dfM=NA, title=NULL )
{
  
  #!! this is an ugly way of doing & i should change
  
  #calculate cumulative age in each age class on each day
  #the rev(seq) bit gives the ages of each age class
  dfCumAgeF <- dfF*rev(seq(dfF[,1])) 
  dfPopF <- data.frame( lapply(dfF,sum) )
  dfTotF <- data.frame( lapply(dfCumAgeF,sum) ) 
  #divide it by total number of individuals to get the mean age 
  dfMeanAgeF <- dfTotF / dfPopF
  #transpose to make plotting easier
  vMeanAgeF <- t(dfMeanAgeF)
    
  #only calculate for males if a 2nd vector is passed
  if ( length(dfM) > 1) #!is.na(dfM) )
  {
    #calculate cumulative age in each age class on each day
    #the rev(seq) bit gives the ages of each age class
    dfCumAgeM <- dfM*rev(seq(dfM[,1])) 
    dfPopM <- data.frame( lapply(dfM,sum) )
    dfTotM <- data.frame( lapply(dfCumAgeM,sum) ) 
    #divide it by total number of individuals to get the mean age 
    dfMeanAgeM <- dfTotM / dfPopM
    #transpose to make plotting easier
    vMeanAgeM <- t(dfMeanAgeM)
    
    ylim <- c(min(c(vMeanAgeF,vMeanAgeM)), max(c(vMeanAgeF,vMeanAgeM)))   
  } else
  {
    #if only 1 object is passed just calc ylim from that
    ylim <- c(min(c(vMeanAgeF)), max(c(vMeanAgeF)))
  }

  #plot line for the first object  
  plot(vMeanAgeF, type='l', xlab='day', ylab='mean age', col="red", lty="dashed", main=title, ylim=ylim )

  #if a 2nd object has been passed add line & legend
  if ( length(dfM) > 1) #!is.na(dfM) )
  {
    lines(vMeanAgeM, lty="dotted", col='blue')
    legend("top", legend=c("Female","Male"), col=c("red","blue"), lty=c(2,3), horiz=TRUE, bty="n" )
  }
  
}
