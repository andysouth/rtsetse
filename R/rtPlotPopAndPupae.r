#' plots popn and pupae over multiple days
#' 
#' \code{rtPlotPopAndPupae} plots change in pop from passed dataframes 
#' for males and females and pupae 

#' @param dfF a dataframe of Males with days in columns and ages in rows
#' @param dfM a dataframe of Females with days in columns and ages in rows
#' @param dfPupaF a dataframe of Pupa Males with days in columns and ages in rows
#' @param dfPupaM a dataframe of Pupa Females with days in columns and ages in rows 
#' @param title a title for the plot  
#' 
#' @return nothing
#' @examples
#' tst <- rtPhase1Test3()
#' rtPlotPopAndPupae(tst$dfRecordF, tst$dfRecordM, tst$dfRecordPupaF, tst$dfRecordPupaM)

#' @export

rtPlotPopAndPupae <- function( dfF,
                               dfM,
                               dfPupaF,
                               dfPupaM,
                               title=NULL )
{
  #calculate the sum of all the ages on each day
  dfTotF <- data.frame( lapply(dfF,sum) )
  dfTotM <- data.frame( lapply(dfM,sum) )
  dfTotPupaF <- data.frame( lapply(dfPupaF,sum) )
  dfTotPupaM <- data.frame( lapply(dfPupaM,sum) )
  
  #transpose to make plotting easier
  vTotAdult <- t(dfTotF + dfTotM)
  vTotPupa <- t(dfTotPupaF + dfTotPupaM)  
  vTotF <- t(dfTotF)  
  vTotM <- t(dfTotM)  
  
  plot(vTotAdult, type='l', xlab='day', ylab='numbers', col="black", main=title, ylim=c(0,max(vTotAdult)))
  lines(vTotPupa, col='green')
  lines(vTotF, col='red',lty='dashed')
  lines(vTotM, col='blue',lty='dotted' )
  
  legend("top", legend=c("adults","F","M","pupae"), col=c("black","red","blue","green"), lty=c(1,2,3,1), horiz=TRUE, bty='n')
  
}
#to test
#rtPlotPopAndPupae(tst$dfRecordF, tst$dfRecordM, tst$dfRecordPupaF, tst$dfRecordPupaM)
