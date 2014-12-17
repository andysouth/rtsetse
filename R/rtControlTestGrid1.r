#' first test of tsetse control
#' 
#' \code{rtControlTestGrid1} simply kills off a constant proportion of adults.  
#' In a central control area.  

#' @param aGrid array of y,x,sex,age
#' @param pControl proportion of adults to be killed by control (daily)
#' @param iControlBorder the number of cells around the edge where no control applied
#' if there are insufficient cells in the passed grid, then it applies control to all cells
#' (this allows small tests to be run)  
#' 
#' @return aGrid updated array of y,x,sex,age
#' @examples
#' aRecord <- rtPhase2Test2()
#' aGrid <- rtGetFromRecord(aRecord,days=1)
#' aGridControlled <- rtControlTestGrid1(aGrid) 
#' @export

rtControlTestGrid1 <- function(aGrid, pControl=0.1, iControlBorder=8)
{

  
  ny <- dim(aGrid)[1]
  nx <- dim(aGrid)[2]
  
  #if there are insufficient cells in the passed grid, 
  #then it applies control to all cells
  if (iControlBorder*2 >= ny) ys4control <- TRUE
  else ys4control <- c((iControlBorder+1):(ny-iControlBorder))
  if (iControlBorder*2 >= nx) xs4control <- TRUE
  else xs4control <- c((iControlBorder+1):(nx-iControlBorder))  
  
  fFliesBeforeControl <- sum(aGrid)
  
  #multiply all age classes within the control area by 1-pControl
  aGrid[ys4control, xs4control,,] <- aGrid[ys4control, xs4control,,] * (1-pControl)
  
  fFliesAfterControl <- sum(aGrid)
  
  #I could print the number of flies killed just for info
  if (verbose) cat("flies killed : ", fFliesBeforeControl-fFliesAfterControl)
  
  
  #return
  invisible(aGrid)
}