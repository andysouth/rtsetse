#' first test of tsetse control
#' 
#' \code{rtControlGrid} simply kills off a constant proportion of adults.  
#' In a central control area.  

#' @param aGrid array of y,x,sex,age
#' @param vpControlF proportion by age of F to be killed by control (daily)
#' age & sex dependent alternative to pControl
#' @param vpControlM proportion by age of M to be killed by control (daily)
#' age & sex dependent alternative to pControl
#' @param pControl control independent of age & sex, proportion of adults to be killed by control (daily)
#' age & sex INdependent alternative to vpControlF & vpControlM
#' @param mGridControlModifier an optional grid to modify control. 
#' The control probability is multiplied by this. SO can use 0,1 grid for control/no control option.
#' @param iControlBorder the number of cells around the edge where no control applied
#' if there are insufficient cells in the passed grid, then it applies control to all cells
#' (this allows small tests to be run)  
#' @param verbose print what it's doing T/F
#' 
#' @return aGrid updated array of y,x,sex,age
#' @examples
#' aGrid <- rtCreateGrid(nY=3,nX=4,nAge=2,fill=1)
#' aGridControlled <- rtControlGrid(aGrid, pControl=0.1) 
#' aGridControlled <- rtControlGrid(aGrid, pControl=0.1, iControlBorder=1) 
#' #testing age & sex dependent control
#' aGridControlled <- rtControlGrid(aGrid, vpControlF=c(0.1,0.2), vpControlM=c(0.3,0.4))
#' @export

rtControlGrid <- function(aGrid, 
                          vpControlF = NULL,
                          vpControlM = NULL,
                          pControl = NULL,
                          mGridControlModifier=NULL,
                          iControlBorder=0, 
                          verbose = TRUE)
{

  #TODO check that the dimensions of vpControlF & M match aGrid
  
  #another check
  if ( !is.null(vpControlF) & !is.null(vpControlM) & !is.null(pControl) )
    warning("You have specified both age dependent (vpControlF & M) and independent (pControl) control, the former will be used\n")
  
  
  nY <- dim(aGrid)[1]
  nX <- dim(aGrid)[2]

  fFliesBeforeControl <- sum(aGrid)
  
  #start by filling control matrix with 1s
  mGridControl <- matrix(1, nrow=nY, ncol=nX)
  
  #if a control modification grid is specified
  if (! is.null(mGridControlModifier))
  {
    #check that dimensions of grid & control grid are the same
    if ( !identical( dim(mGridControl), dim(mGridControlModifier) ) )
      stop("dimensions of gridControl and gridControlModifier are not the same")
    
    mGridControl <- mGridControlModifier * mGridControl
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
  
  if ( !is.null(vpControlF) & !is.null(vpControlM))
  {    
    #age & sex dependent control
    
    #testing const mort just on F : seems to work
  #   aGridF <- aGrid[,,"F",]
  #   aGridF2 <- plyr::aaply(aGridF,.margins=c(3), .drop=FALSE,function(m) (m-(m*pControl*mGridControl)))
  #   aGridF2 <- aperm(aGridF2, c(2,3,1))
    
    #BUT another way of doing could be to call rtMortality()
    for(x in seq_along(dimnames(aGrid)$x)){
      for(y in seq_along(dimnames(aGrid)$y)){
        #only apply mortality if flies in cell (to save time)
        if ( sum(aGrid[y,x,,]) > 0 )
        { 
          #modify the age & sex mortality vectors by the grid
          vpMortF <- vpControlF * mGridControl[y,x]
          vpMortM <- vpControlM * mGridControl[y,x]
          
          #apply mortality for this cell
          aGrid[y,x,,] <- rtMortality(vFem = aGrid[y,x,'F',],
                                      vMal = aGrid[y,x,'M',],
                                      vpMortF = vpMortF,
                                      vpMortM = vpMortM,
                                      returnArray = TRUE,
                                      propDD = 0 )  
        }#end flies in cell
      }#end y
    }#end x
  #end age & sex dependent control  
  } else if (! is.null(pControl))
  {  
    #if control is specified as a single age/sex independent value
  
    #for each cell pop = pop - (pop*controlMort*controlModifier) 
    #control modifier passed as a grid (mGridControl)
    aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) (m-(m*pControl*mGridControl)))
    #put array dimensions back in correct order
    aGrid <- aperm(aGrid, c(3,4,1,2))
    
  } else 
  {
    stop("you need to specify either a single control mortality or age & sex dependent vectors")
  }
  
  
  fFliesAfterControl <- sum(aGrid)
  
  #I could print the number of flies killed just for info
  if (verbose) cat("flies killed : ", fFliesBeforeControl-fFliesAfterControl,"\n")
  
  
  #return
  invisible(aGrid)
}