#' movement to neighbouring cells with reflecting area boundaries. 
#' 
#' \code{rtMove} moves proportion of popn in each cell to the 4 neighbouring cells can be dependent on vegetation dependent on arguments.

#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[y,x,age] to aaply(.margins=3).  
#' Optional arguments allow the function to be used in different ways.  
#' 
#' Argument options :  
#' 
#' 1 \code{m}, \code{pMove} : Simple movement with reflecting boundaries.  
#' 
#' 2 \code{m}, \code{pMove}, \code{mNog} : Avoidance of no-go areas.
#' 
#' 3 Vegetation dependent movement :\cr
#' \code{m}, \code{pMove}, \code{aVegMoveMult} : Probabilities precalculated for speed by \code{\link{rtSetVegMoveGrids}}.\cr
#' \code{m}, \code{pMove}, \code{mVegMove} : Probabilities from a matrix. Slower if called repeatedly.\cr
#' \code{m}, \code{pMove}, \code{mVegCats}, \code{dfMoveByVeg} : Probabilities calculated from vegetation categories and a lookup.\cr
#'         
#' 4 Movement influenced by difference in vegetation between source and sink cells :\cr         
#' \code{m}, \code{pMove}, \code{aVegDifMult} : Probabilities precalculated for speed by \code{\link{rtSetVegDifGrids}}.\cr
#' \code{m}, \code{pMove}, \code{mVegCats}, \code{iBestVeg} : Probabilities calculated from vegetation categories and the best vegetation index.\cr
#' 
#' Options from 2,3 & 4 can be combined. See \code{vignette("vignette-movement", package="rtsetse")}
#' 
#' Doesn't try to cope with nrow or ncol==1.

#' @param m a matrix of cells containing a single number representing one age
#' @param mNog optional matrix of cells of 0&1, 0 for nogo areas 
#' @param mVegMove optional matrix of vegetation movement modifiers >1 increases movement out of the cell, <1 decreases movement out of the cell 
#' @param aVegMoveMult optional array of grids used internally to represent movement dependent on vegetation, can be created by \code{\link{rtSetVegMoveGrids}} 
#' @param mVegCats optional matrix of vegetation categories
#' @param dfMoveByVeg dataframe specifying a movement multiplier for each vegetation category 
#' @param iBestVeg optional preferred vegetation number (1-5) for this species 
#' @param aVegDifMult optional array of grids used internally to represent movement across vegetation boundaries, can be created by \code{\link{rtSetVegDifGrids}}
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' rtMove(verbose=TRUE)
#' @seealso \code{vignette("vignette-movement", package="rtsetse")}
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
  #it may not be needed at all, the vegetation difference modifiers may cope with it
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
  #if the array & probMatrix is not passed the array can be calculated from the mVegCats (categories)  
  if (is.null(aVegMoveMult) & is.null(mVegMove) & !is.null(dfMoveByVeg) & !is.null(mVegCats))
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
  if (!is.null(aVegMoveMult))
  {
    indicesHighMove <- which((aVegMoveMult[,,'here']*pMove > 1))
    if (length(indicesHighMove) >0)
    {
      warning("your combination of pMove and vegetation movement multipliers causes ",length(indicesHighMove),
              " cells to have proportion moving >1, these will be set to 1 and all will move out")
      #reduce multiplier in cells so that the result will be 1 (all move)
      aVegMoveMult[,,'here'][indicesHighMove] <- 1/pMove
    }     
  }
 
  
  #effect of differences in vegetation between cells on movement   
  #if the array is not passed it can be calculated from the mVegCats (categories)
  if (is.null(aVegDifMult) & !is.null(iBestVeg) & !is.null(mVegCats))
  {
    aVegDifMult <- rtSetVegDifMoveGrids( mVegCats = mVegCats, iBestVeg = iBestVeg )
  }

  
  #BEWARE 5/3/15 THIS IS ONE OF THE TRICKIEST BITS IN THE WHOLE OF RTSETSE

  #calc arrivers in a cell from it's 4 neighbours, and stayers that don't go
  
  #old versions if no vegetation effects
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  #mStayers <- (1-pMove)*m  
  
  #if both vegetation and vegetation difference effects
  if ( !is.null(aVegDifMult) & !is.null(aVegMoveMult) )
  {
    mArrivers <- pMove*mNog*(mN * aVegMoveMult[,,'N'] * aVegDifMult[,,'N'] + 
                             mE * aVegMoveMult[,,'E'] * aVegDifMult[,,'E'] + 
                             mS * aVegMoveMult[,,'S'] * aVegDifMult[,,'S'] + 
                             mW * aVegMoveMult[,,'W'] * aVegDifMult[,,'W'])/4   
    
    #aVegDifMult[,,'NS'] etc. for each cell the difference in preference with 4 neighbours that act as sinks  
    
    mStayers <- m * (1- (pMove * (aVegDifMult[,,'NS'] +
                                  aVegDifMult[,,'EW'] + 
                                  aVegDifMult[,,'SN'] + 
                                  aVegDifMult[,,'WE'])/4) 
                     * aVegMoveMult[,,'here'] * (mNogN + mNogE + mNogS + mNogW)/4 ) 
        
  } else if( !is.null(aVegMoveMult) )
    #just vegetation effect
  {
    mArrivers <- pMove*mNog*(mN * aVegMoveMult[,,'N'] + 
                             mE * aVegMoveMult[,,'E'] + 
                             mS * aVegMoveMult[,,'S'] + 
                             mW * aVegMoveMult[,,'W'] )/4   
 
    mStayers <- m * (1- (pMove  * aVegMoveMult[,,'here'] * (mNogN + mNogE + mNogS + mNogW)/4))   
                     
  }else if( !is.null(aVegDifMult) )
    #just vegetation difference effect
  {
    mArrivers <- pMove*mNog*(mN * aVegDifMult[,,'N'] + 
                             mE * aVegDifMult[,,'E'] + 
                             mS * aVegDifMult[,,'S'] + 
                             mW * aVegDifMult[,,'W'])/4   
    
    #aVegDifMult[,,'NS'] etc. for each cell the difference in preference with 4 neighbours that act as sinks  
    
    mStayers <- m * (1- (pMove * (aVegDifMult[,,'NS'] +
                                  aVegDifMult[,,'EW'] + 
                                  aVegDifMult[,,'SN'] + 
                                  aVegDifMult[,,'WE'])/4) 
                      * (mNogN + mNogE + mNogS + mNogW)/4 )     
  } else
    #neither vegetation effect
    #can include nogo area effects, but if these are set to 1 above they do nothing
  {
    mArrivers <- pMove*mNog*(mN + mE + mS + mW)/4   
    
    mStayers <- m * (1- (pMove * (mNogN + mNogE + mNogS + mNogW)/4 ))        
  }
 

  
    
  
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
    if (!is.null(aVegMoveMult))
    {
      cat("\nveg movement multiplier\n") 
      print(aVegMoveMult[,,'here'])
    }
    if (!is.null(aVegDifMult))
    {
      cat("\nexample of veg dif from preferred between cells (this is N)\n") 
      print(aVegDifMult[,,'N']) 
    }
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



