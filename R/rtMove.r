#' movement 
#' 
#' \code{rtMove} moves proportion of popn in each cell to the 4 neighbouring cells dependent on vegetation.

#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[y,x,age] to aaply(.margins=3).  
#' Optional arguments allow the function to be used in different ways.  
#' 
#' Doesn't try to cope with nrow or ncol==1.

#' @param m a matrix of cells containing a single number representing one age
#' @param mNog optional matrix of cells of 0&1, 0 for nogo areas 
#' @param mVegMove optional matrix of vegetation movement modifiers >1 increases movement out of the cell, <1 decreases movement out of the cell 
#' @param aVegMoveMult optional array of grids used internally to represent movement dependent on vegetation
#' @param mVegCats optional matrix of vegetation categories
#' @param dfMoveByVeg dataframe specifying a movement multiplier for each vegetation category 
#' @param iBestVeg optional preferred vegetation number (1-5) for this species 
#' @param aVegDifMult optional array of grids used internally to represent movement across vegetation boundaries
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' rtMove(verbose=TRUE)
#' @export

rtMove <- function(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)),
                   mNog = NULL,
                   mVegMove = NULL,
                   aVegMoveMult = NULL,
                   mVegCats = NULL,
                   dfMoveByVeg = NULL,
                   iBestVeg = NULL,
                   aVegDifMult = NULL,
                   pMove=0.4,
                   verbose=FALSE) {
  
  
  #this doesn't cope with nrow=1 or ncol=1 see rtMoveIsland() which tries (and i think fails) 
  if( nrow(m) < 2 | ncol(m) < 2 )
    stop("reflecting movement does not work if less than 2 grid rows or columns")
  
  
  #to speed up can just return if there are no popns in matrix
  if ( sum(m)==0 ) return(m)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #in the code below matrices with NESW on end are source cells
  #matrices without are destination cells
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #speed efficient way of doing movement
  #create a copy of the matrix shifted 1 cell in each cardinal direction
  mN <- shiftGridReflectN(m)
  mE <- shiftGridReflectE(m)
  mS <- shiftGridReflectS(m) 
  mW <- shiftGridReflectW(m)  
  
  #creating matrices of neighbouring nogo areas
  #this doesn't need to be repeated every day
  #it could be done at the start of a simulation, and passed probably as a list or array
  #but time cost of doing this for a few 100 days is probably fairly low
  if (!is.null(mNog))
  {
    mNogN <- shiftGridReflectN(mNog)
    mNogE <- shiftGridReflectE(mNog)
    mNogS <- shiftGridReflectS(mNog)  
    mNogW <- shiftGridReflectW(mNog)   
  } else 
  {
    #set all these to 1 so they have no effect on movement calc later
    mNog <- mNogN <- mNogE <- mNogS <- mNogW <- 1
  }
  
  #code to cope with different ways of passing vegetation
  
  #effect of vegetation in a cell on movement 
  
  #if no vegetation movement modifiers
  if (is.null(aVegMoveMult) & is.null(mVegMove) & is.null(dfMoveByVeg))
  {
    dimnames1 <- list(grid=c("here","N","E","S","W"))
    #dim of array got from dimnames above
    #set all these to 1 so they have no effect on movement calc later
    aVegMoveMult <- array(1, dim=sapply(dimnames1,length), dimnames=dimnames1)
    
  } else if (is.null(aVegMoveMult) & is.null(mVegMove) & !is.null(dfMoveByVeg) & !is.null(mVegCats))
    #if the array & probMatrix is not passed the array can be calculated from the mVegCats (categories)
  {
    aVegMoveMult <- rtSetVegMoveGrids( mVegCats = mVegCats, dfMoveByVeg = dfMoveByVeg )
    
  } else if (is.null(aVegMoveMult) & !is.null(mVegMove))
  {
    #or it can be calculated from a matrix of movement probabilities
    aVegMoveMult <- rtSetVegMoveGrids( mVegMove = mVegMove )
  }

  #todo: can I still get this check to work with the new system where difference in vegetation between cells is used too
  #but: a high movement might be counteracted by the vegetation difference effect
  #check for if any cells in pMove*mVegMove are >1
  #if so set to 1 so that all indivs leave
  indicesHighMove <- which((aVegMoveMult[,,'here']*pMove > 1))
  if (length(indicesHighMove) >0)
  {
    warning("your combination of pMove and vegetation movement multipliers causes ",length(indicesHighMove),
            " cells to have proportion moving >1, these will be set to 1 and all will move out")
    #reduce multiplier in cells so that the result will be 1 (all move)
    aVegMoveMult[,,'here'][indicesHighMove] <- 1/pMove
  }  
  
  #effect of differences in vegetation between cells on movement   
  #todo: do the same here for aVegDifMult as for aVegMoveMult above
  

  
  

  
  #BEWARE 5/3/15 THIS IS ONE OF THE TRICKIEST BITS IN THE WHOLE OF RTSETSE

  #rtSetVegMoveGrids : returns aVegMoveMult[NSEW]
  #rtSetVegDifMoveGrids : returns aVegDifMult[NSEW,NS,EW etc.]
  
  #calc arrivers in a cell from it's 4 neighbours
  
  #old version if no vegetation effects
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  
  mArrivers <- pMove*mNog*(mN * aVegMoveMult[,,'N'] * aVegDifMult[,,'N'] + 
                           mE * aVegMoveMult[,,'E'] * aVegDifMult[,,'E'] + 
                           mS * aVegMoveMult[,,'S'] * aVegDifMult[,,'S'] + 
                           mW * aVegMoveMult[,,'W'] * aVegDifMult[,,'W'])/4   
  
  #old version if no vegetation effects
  #mStayers <- (1-pMove)*m  

  #aVegDifMult[,,'NS'] etc.
  #for each cell they are the difference in preference with 4 neighbours that act as sinks  
  
  mStayers <- m * (1- (pMove * (aVegDifMult[,,'NS'] +aVegDifMult[,,'EW'] + aVegDifMult[,,'SN'] + aVegDifMult[,,'WE'])/4) 
                * aVegMoveMult[,,'here'] * (mNogN + mNogE + mNogS + mNogW)/4 )  
  
  
  #number of flies in all cells is a sum of those that 
  #arrived and those that stayed
  mNew <- mArrivers + mStayers
  
  #this avoids duplicate levels problems outside the function
  dimnames(mNew) <- dimnames(m)
  
  # cat("\nmNog\n") 
  # print(mNog)
  
  if (verbose)
  {
    cat("popn before\n") 
    print(m)
    cat("\nno-go areas (0=nogo)\n") 
    print(mNog)
    cat("\nveg movement multiplier\n") 
    print(aVegMoveMult[,,'here'])
    cat("\nveg dif from preferred\n") 
    print(mVegDifPref)
    cat("\nmStayers\n") 
    print(mStayers)
    cat("\nmArrivers\n") 
    print(mArrivers)
    cat("\nmNew\n") 
    print(mNew)
  }
  
  #one way of testing this is that the total number of flies shouldn't have changed
  #(i think reflecting edges mean should get same in as out)
  #float rounding cause small differences, this checks for differences >1 %
  if ( (abs(sum(m)-sum(mNew))/sum(m) ) > 0.01)
    warning("in rtMove() num flies seems to have changed during movement, before=",sum(m)," after=",sum(mNew),"\n")
  
  
  invisible( mNew )
}

#non exported helper functions

#~~~reflecting movement helper functions~~~

#' shift a matrix one cell N, copy boundary to represent reflection
#' @param m matrix
#' @return shifted matrix
shiftGridReflectN <- function(m) { mnew <- rbind( m[1,], m[-nrow(m),] ) }

#' shift a matrix one cell E, copy boundary to represent reflection
#' @param m matrix
#' @return shifted matrix
shiftGridReflectE <- function(m) { mnew <- cbind( m[,-1], m[,ncol(m)] ) }

#' shift a matrix one cell S, copy boundary to represent reflection
#' @param m matrix
#' @return shifted matrix
shiftGridReflectS <- function(m) { mnew <- rbind( m[-1,], m[nrow(m),] ) }

#' shift a matrix one cell W, copy boundary to represent reflection
#' @param m matrix
#' @return shifted matrix
shiftGridReflectW <- function(m) { mnew <- cbind( m[,1], m[,-ncol(m)] ) }

#~~~island movement helper functions~~~

#' shift a matrix one cell N
#' fill empty cells with 0 for island model (or passed value)
#' @param m matrix
#' @param fill value to put in empty cells, default=0 for island model
#' @return shifted matrix
shiftGridIslandN <- function(m, fill) { mnew <- rbind( rep(fill, ncol(m)), m[-nrow(m),] ) }

#' shift a matrix one cell E
#' fill empty cells with 0 for island model (or passed value)
#' @param m matrix
#' @param fill value to put in empty cells, default=0 for island model
#' @return shifted matrix
shiftGridIslandE <- function(m, fill) { mnew <- cbind( m[,-1], rep(fill, nrow(m)) ) }

#' shift a matrix one cell S
#' fill empty cells with 0 for island model (or passed value)
#' @param m matrix
#' @param fill value to put in empty cells, default=0 for island model
#' @return shifted matrix
shiftGridIslandS <- function(m, fill) { mnew <- rbind( m[-1,], rep(fill, ncol(m)) ) }

#' shift a matrix one cell W
#' fill empty cells with 0 for island model (or passed value)
#' @param m matrix
#' @param fill value to put in empty cells, default=0 for island model
#' @return shifted matrix
shiftGridIslandW <- function(m, fill) { mnew <- cbind( rep(fill, nrow(m)), m[,-ncol(m)] ) }



