#' to return an array of grids used internally to represent movement dependent on vegetation 
#' 
#' Used to represent effect of vegetation in a cell on movement out of that cell. 
#' Returns an array of movement multiplier grids to N,S,E & W

#' @param mVegCats a matrix of vegetation categories
#' @param dfMoveByVeg dataframe specifying a movement multiplier for each vegetation category 
#' @param verbose print what it's doing T/F
#' 
#' @return an array of movement multiplier grids 'here' and to N,S,E & W

#2 functions
#rtSetVegMoveGrids : returns aVegMoveMult[NSEW]
#rtSetVegDifMoveGrids : returns aVegDifMult[NSEW,NS,EW etc.]

rtSetVegMoveGrids <- function(mVegCats = array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4)),
                              dfMoveByVeg = data.frame(code=c("D","T","O","S","B","G","N"),move=c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 0)),
                              verbose=FALSE) {

  
  mVegMove <- rtSetGridFromVeg( mVegetation=mVegCats, dfLookup=dfMoveByVeg )
  
  
  nY <- dim(mVegCats)[1]
  nX <- dim(mVegCats)[2]
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