

#I could put the mVegMove stuff in here too
#but then how would I name the storage in the returned array
#does it become less clear ?

#I could have 2 functions
#rtSetVegMoveGrids : returns aVegMoveMult[NSEW]
#rtSetVegDifMoveGrids : returns aVegDifMult[NSEW,NS,EW etc.]


rtSetVegMoveGrids <- function(mVegCats = array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4)),
                              dfMoveByVeg =  data.frame(code=c("D","T","O","S","B","G","N"),move=c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 0))
                           iBestVeg = 4,
                           verbose=FALSE) {

  
  mVegMove <- rtSetGridFromVeg( mVegetation=mVegCats, dfLookup=dfMoveByVeg )
  
  
  nY <- dim(mVegCats)[1]
  nX <- dim(mVegCats)[2]
  dimnames1 <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), grid=c("N","E","S","W"))

  #dim of array got from dimnames above
  aVegMoveMult <- array(dim=sapply(dimnames1,length), dimnames=dimnames1)
     
  #todo?? I might need a aVegMoveMult[,,"here"] <- mVegMove
  
  aVegMoveMult[,,"N"] <- shiftGridReflectN(mVegMove)
  aVegMoveMult[,,"E"] <- shiftGridReflectE(mVegMove)
  aVegMoveMult[,,"S"] <- shiftGridReflectS(mVegMove)
  aVegMoveMult[,,"W"] <- shiftGridReflectW(mVegMove)
  

  invisible(aVegDifMult)
}