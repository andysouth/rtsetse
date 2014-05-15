#' plots popn over multiple days
#' \code{rtPlotPop} plots change in pop frompassed single dataframe
#' you would have to pass 2 dataframes (for M&F to get total popn)

#' @param df a dataframe with days in columns and ages in rows
#' @param title, a title for the plot  
#' 
#' @return nothing
#' @export

rtPlotPop <- function( df, title=NULL )
{
  #calculate the sum of all the ages on each day
  dfPop <- data.frame( lapply(df,sum) )
  
  #transpose to make plotting easier
  vPop <- t(dfPop)
  
  plot(vPop, type='l', xlab='day', ylab='numbers',main=title)
  
}