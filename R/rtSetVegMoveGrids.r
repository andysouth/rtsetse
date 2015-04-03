#' to return an array of grids used internally to represent movement dependent on vegetation 
#' 
#' Used to represent effect of vegetation in a cell on movement out of that cell. 
#' Returns an array of movement multiplier grids to N,S,E & W

#' @param mVegCats a matrix of vegetation categories
#' @param dfMoveByVeg dataframe specifying a movement multiplier for each vegetation category 
#' @param mVegMove optional matrix of vegetation movement modifiers >1 increases movement out of the cell, <1 decreases movement out of the cell 
#' @param verbose print what it's doing T/F
#' 
#' @return an array of movement multiplier grids 'here' and to N,S,E & W
#' 
#' @seealso \code{\link{rtSetVegDifGrids}} which sets up similar grids for influencing movement according to vegetation differences between cells.\cr
#' \code{\link{rtMove}} uses the grids created.\cr\cr
#' The movement vignette contains more details about how movement can be represented, type this in the R console : 
#' \code{vignette("vignette-movement", package="rtsetse")}
#' 
rtSetVegMoveGrids <- function(mVegCats = array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4)),
                              dfMoveByVeg = data.frame(code=c("D","T","O","S","B","G","N"),move=c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 0)),
                              mVegMove = NULL,
                              verbose=FALSE) {

  #todo add defensive check for if both mVegCats & mVegMove are passed
  
  #if a movement matrix is specified that is used in preference to the vegetation map
  if ( is.null(mVegMove) )
     mVegMove <- rtSetGridFromVeg( mVegetation=mVegCats, dfLookup=dfMoveByVeg )
  
  
  nY <- dim(mVegMove)[1]
  nX <- dim(mVegMove)[2]
  dimnames1 <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), grid=c("here","N","E","S","W"))

  #dim of array got from dimnames above
  aVegMoveMult <- array(dim=sapply(dimnames1,length), dimnames=dimnames1)
     
  #save the movement multipliers for the cells
  aVegMoveMult[,,"here"] <- mVegMove
  #and for their neighbours
  aVegMoveMult[,,"N"] <- shiftGridReflectN(mVegMove)
  aVegMoveMult[,,"E"] <- shiftGridReflectE(mVegMove)
  aVegMoveMult[,,"S"] <- shiftGridReflectS(mVegMove)
  aVegMoveMult[,,"W"] <- shiftGridReflectW(mVegMove)
  

  invisible(aVegMoveMult)
}