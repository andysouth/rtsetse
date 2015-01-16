#' first test of tsetse control
#' 
#' \code{rtControlGrid} simply kills off a constant proportion of adults.  
#' In a central control area.  

#' @param aGrid array of y,x,sex,age
#' @param pControl proportion of adults to be killed by control (daily)
#' @param aGridControlModifier an optional grid to modify control. 
#' The control probability is multiplied by this. SO can use 0,1 grid for control/no control option.
#' @param iControlBorder the number of cells around the edge where no control applied
#' if there are insufficient cells in the passed grid, then it applies control to all cells
#' (this allows small tests to be run)  
#' @param verbose print what it's doing T/F
#' 
#' @return aGrid updated array of y,x,sex,age
#' @examples
#' aGrid <- rtCreateGrid(nY=3,nX=4,nAge=2,fill=1)
#' aGridControlled <- rtControlGrid(aGrid) 
#' aGridControlled <- rtControlGrid(aGrid, iControlBorder=1) 
#' @export

rtControlGrid <- function(aGrid, 
                               pControl=0.1, 
                               aGridControlModifier=NULL,
                               iControlBorder=0, 
                               verbose = TRUE)
{

  
  nY <- dim(aGrid)[1]
  nX <- dim(aGrid)[2]

  fFliesBeforeControl <- sum(aGrid)
  
  #start by filling control matrix with 1s
  mGridControl <- matrix(1, nrow=nY, ncol=nX)
  
  #if a control modification grid is specified
  if (! is.null(aGridControlModifier))
  {
    #check that dimensions of grid & control grid are the same
    if ( !identical( dim(aGrid), dim(aGridControlModifier) ) )
      stop("dimensions of grid and GridControlModifier are not the same")
    
    mGridControl <- aGridControlModifier * mGridControl
  }  
  
  #if a border is specified
  if ( iControlBorder > 0 )
  {
    #border is only applied if space for it
    if ( iControlBorder < nY/2 )
    {
      ysBorder <- c( c(1:iControlBorder),c((nY+1-iControlBorder):nY))
      #don't have control in the border
      mGridControl[ysBorder,] <- 0      
    } else warning("no space for control border")
    
    if ( iControlBorder < nX/2 )
    {   
      xsBorder <- c( c(1:iControlBorder),c((nX+1-iControlBorder):nX))
      #don't have control in the border
      mGridControl[, xsBorder] <- 0
    } else warning("no space for control border")
  }
  
  
  #TODO I probably want to put some of the below into a function on it's own
  
  #for each cell pop = pop - (pop*controlMort*controlModifier) 
  #but this actually passes the control modifier as a grid (mGridControl)
  aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) (m-(m*pControl*mGridControl)))
  #put array dimensions back in correct order
  aGrid <- aperm(aGrid, c(3,4,1,2))
  
  
  fFliesAfterControl <- sum(aGrid)
  
  #I could print the number of flies killed just for info
  if (verbose) cat("flies killed : ", fFliesBeforeControl-fFliesAfterControl,"\n")
  
  
  #return
  invisible(aGrid)
}