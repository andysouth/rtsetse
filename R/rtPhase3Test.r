#' a test of feeding (phase3) across multiple hunt periods 
#'
#' \code{rtPhase3Test} tests feeding for a single group of flies in a single hunger cycle.
#' The user can set the number of hunt periods, in the main model the plan
#' is to set the number of hunt periods at 15 for age1 and 30 for subsequent hunger cycles.

#' @param iNumHuntPeriods number of hunt periods
#' @param fHunters the number of hunting flies in this cell,age,sex and hunt period
#' @param pDetectMan probability of detecting one human per km2
#' @param pDetectOxe probability of detecting one other host (ox equivalent) per km2
#' @param pFeedMan probability of feeding on a human once detected 
#' @param pFeedOxe probability of feeding on other host (ox equivalent) once detected 
#' @param fDensityMan density of humans per km2 in tsetse habitat 
#' @param fDensityOxe density of other hosts (ox equivalents) per km2 in tsetse habitat 
#' @param testing whether to output testing messages to the console 
#' @param report filename for a report for this run, if not specified no report is produced

#' @return dataframe of manFeeders and remaining hunters, length the number of hunt periods
#' @examples
#' \dontrun{
#' dF <- rtPhase3Test()
#' }
#' @export
#' 
rtPhase3Test <- function( iNumHuntPeriods=15,
                          fHunters=1000,
                          pDetectMan=0.001,
                          pDetectOxe=0.005,
                          pFeedMan=0.1,
                          pFeedOxe=0.8,
                          fDensityMan=1,
                          fDensityOxe=10,
                          testing = TRUE,
                          report = NULL ) #"reportPhase3.html" ) 
{
  
  ##some argument checking
  #if( nRow < 2 | nCol < 2 )
  #  stop("movement does not work if less than 2 grid rows or columns")
  
  #to collect the results over multiple hunt periods
  #to enable easy plotting
  #starts at 0
  dF <- data.frame(huntPeriod=c(0:iNumHuntPeriods),
                   hunters=0,
                   manFeeders=0,
                   manFeedersCum=0)
  
  #setting hunters in period 0 to the start value
  #!beware that huntPeriod0 goes in element 1 of the dataframe
  dF$hunters[1] <- fHunters
  
  #starting cumulative manFeeder counter
  fManFeedersCum <- 0
  
  for( iHuntPeriod in 1:iNumHuntPeriods ) {
    
    #call function for one hunt period
    lF <- rtFeedingOneHuntPeriod(fHunters=fHunters, 
                                 pDetectMan = pDetectMan,
                                 pDetectOxe = pDetectOxe,
                                 pFeedMan = pFeedMan,
                                 pFeedOxe = pFeedOxe,
                                 fDensityMan = fDensityMan,
                                 fDensityOxe = fDensityOxe,                                           
                                 testing = testing )
    
    #incrementing cumulative flies feeding on humans
    fManFeedersCum <- fManFeedersCum + lF$fManFeeders
    
    #iHuntPeriod+1 because starting numbers go in element1 of the dataframe
    #dF$huntPeriod[iHuntPeriod+1] <- iHuntPeriod
    dF$hunters[iHuntPeriod+1] <- lF$fHunters    
    dF$manFeeders[iHuntPeriod+1] <- lF$fManFeeders
    dF$manFeedersCum[iHuntPeriod+1] <- fManFeedersCum
    
    #to go into the next period
    fHunters <- lF$fHunters
    
  } #end of hunt periods loop
  

  
  #produce a report on the model run, with text & graphs
  #if (length(report)>0) rtReportPhase2( aRecord=aRecord, lNamedArgs=lNamedArgs, filename=report )
  
  
  #returning 
  invisible(dF)
}


