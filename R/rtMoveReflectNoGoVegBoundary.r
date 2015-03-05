#' movement affected by vegetation and NoGo areas, to cells NESW, reflecting boundaries
#' 
#' \code{rtMoveReflectNoGoVegBoundary} moves proportion of popn in each cell to the 4 neighbouring cells.
#' The number of movers out is influenced by vegetation.
#' Movers are divided equally between the 4 cardinal neighbours.
#' If any of the neighboring cells are no-go areas the flies that would have moved there
#' stay in their current cell. Thus movement to the other neighbouring cells will not be increased in this time step.
#' But it will be increased in following time steps because the neighbouring cells will receive a proportion of the flies 
#' that didn't move to the nogo area in the preceeding timestep. 
#' This could represent flies turning back from an unpleasant area in one timestep and then trying other directions later.
#' Boundaries are reflecting.
#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[y,x,age] to aaply(.margins=3)
#' Doesn't try to cope with nrow or ncol==1.

#' @param m a matrix of cells containing a single number representing one age
#' @param mNog a matrix of cells of 0&1, 0 for nogo areas 
#' @param mVeg a matrix of vegetation movement modifiers >1 increases movement out of the cell, <1 decreases movement out of the cell 
#' @param mVegCats a matrix of vegetation categories
#' @param iBestVeg which is the preferred vegetation number (1-5) for this species 
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' #1 nogo neighbour
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #2 nogo neighbours
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #3 nogo neighbours
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #4 nogo neighbours, all flies stay
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' @export

rtMoveReflectNoGoVegBoundary <- function(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)),
                                 mNog = NULL,
                                 mVeg = NULL,
                                 mVegCats = array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4)),
                                 iBestVeg = 4,
                                 pMove=0.4,
                                 verbose=FALSE) {
  
  
  #!beware that this doesn't cope with nrow=1 or ncol=1 
  #see rtMoveIsland() which tries (and i think fails) to sort 
  #tricky to work out, R treats vectors and matrices differently
  
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
  #these have now been replaced by the shiftGrid* functions
  #island model uses 0's
  #mN = rbind( rep(0,ncol(m)), m[-nrow(m),] )
  #mE = cbind( m[,-1], rep(0,nrow(m)) )
  #mS = rbind( m[-1,], rep(0,ncol(m)) )
  #mW = cbind( rep(0,nrow(m)), m[,-ncol(m)] )
  #reflecting boundaries
  #0's from island model above are replaced with a copy of boundary row or col
  #mN = rbind( m[1,], m[-nrow(m),] )
  #mE = cbind( m[,-1], m[,ncol(m)] )
  #mS = rbind( m[-1,], m[nrow(m),] ) 
  #mW = cbind( m[,1], m[,-ncol(m)] )  
  
  #change to use of functions
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
  
  
  #vegetation movement modifiers from source cells
  if (!is.null(mVeg))
  {
    mVegN <- shiftGridReflectN(mVeg)
    mVegE <- shiftGridReflectE(mVeg)
    mVegS <- shiftGridReflectS(mVeg)   
    mVegW <- shiftGridReflectW(mVeg)    
  } else 
  {
    #set all these to 1 so they have no effect on movement calc later
    mVeg <- mVegN <- mVegE <- mVegS <- mVegW <- 1
  }
  
  
  #check for if any cells in pMove*mVeg are >1
  #if so set to 1 so that all indivs leave
  indicesHighMove <- which((mVeg*pMove > 1))
  if (length(indicesHighMove) >0)
  {
    warning("your combination of pMove and vegetation movement multipliers causes ",length(indicesHighMove),
            " cells to have proportion moving >1, these will be set to 1 and all will move out")
    #reduce multiplier in cells so that the result will be 1 (all move)
    mVeg[indicesHighMove] <- 1/pMove
  }
  
  
  ######################################
  #add a decrease in movement from 'better' to 'poorer' vegetation types as in Hat-trick
  
  #calculate change associated with each move, e.g. mVegN-mVeg for source-destination
  
  #can I calculate once based on the vegmap and save results as an array ?
  #aVegChange[y,x,(N,E,S,W)]
  
  #vegetation types are ordered by decreasing density 1 to 5
  #movement that results in a change away from preferred density is reduced
  #by the number of categories of the change
  
  #algorithm :
  #qualityChange = abs(from-best) - abs(to-best)
  #if (qualityChange) >= 0 move=100%
  #else if (qualityChange < 0) decrease movement
  #default decrease in movement from Hat-trick is
  #1, 0.3, 0.1, 0.03, 0.01, 0.001 
  
  
  #convert veg characters to numeric
  mVegNum <- rtSetGridFromVeg( mVegCats, dfLookup=data.frame(from=c("D","T","O","S","B","G","N"),to=c(1,2,3,4,5,6,999),stringsAsFactors = FALSE ))

  #matrix of difference of veg in cell from best
  mVegDifPref <- abs(iBestVeg-mVegNum)
  
  #remember
  #in the code below matrices with NESW on end are source cells
  #matrices without are destination cells
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #so I need to create NESW matrices that represent the change in veg preference associated with that move
  #first create copies
  mVegDifPrefN <- shiftGridReflectN(mVegDifPref)
  mVegDifPrefE <- shiftGridReflectE(mVegDifPref)
  mVegDifPrefS <- shiftGridReflectS(mVegDifPref)   
  mVegDifPrefW <- shiftGridReflectW(mVegDifPref)      
  #then do calculation, source-destination
  mVegDifPrefN <- mVegDifPrefN - mVegDifPref
  mVegDifPrefE <- mVegDifPrefE - mVegDifPref
  mVegDifPrefS <- mVegDifPrefS - mVegDifPref
  mVegDifPrefW <- mVegDifPrefW - mVegDifPref
  #seems to work
  #unique(as.vector(mVegDifPrefN))
  #now convert the change associated with the move to a modifier of the movement rate
  #convert 0:5 to   1, 0.3, 0.1, 0.03, 0.01, 0.001 
  #mVegbmult mVeg boundary multiplier
  #todo write dryer version of this & previous similar actions
  mVegbmultN <- rtSetGridFromVeg( mVegDifPrefN, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mVegbmultE <- rtSetGridFromVeg( mVegDifPrefE, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mVegbmultS <- rtSetGridFromVeg( mVegDifPrefS, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mVegbmultW <- rtSetGridFromVeg( mVegDifPrefW, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  
  #these go the opposite way
  #and all boundary values are replaced with 1
  #because for reflecting boundaries there will be no change in vegetation 
  #associated with movements in and out of the grid    

  mVegbmultSN <- shiftGridIslandN( mVegbmultS, fill=1 )
  mVegbmultWE <- shiftGridIslandE( mVegbmultW, fill=1 )
  mVegbmultNS <- shiftGridIslandS( mVegbmultN, fill=1 )   
  mVegbmultEW <- shiftGridIslandW( mVegbmultE, fill=1 )   
  
                                   
  #calc arrivers in a cell from it's 4 neighbours
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  
  #add that movers aren't received at a cell if it is nogo
  #below is version used in rtMoveRelfectNoGo
  #mArrivers <- pMove*(mN*mNog + mE*mNog + mS*mNog + mW*mNog)/4  
  
  #mNog at the destination cell that matters, and mVeg at the source cell
  #uses mVegN & mN etc for source cells, mNog for the destination cells 
  #mArrivers <- pMove*(mN*mVegN*mNog + mE*mVegE*mNog + mS*mVegS*mNog + mW*mVegW*mNog)/4 
  #this is equivalent to above and simpler
  #mArrivers <- pMove*mNog*(mN*mVegN + mE*mVegE + mS*mVegS + mW*mVegW)/4    
  #adding boundary effects
  mArrivers <- pMove*mNog*(mN*mVegN*mVegbmultN + mE*mVegE*mVegbmultE + mS*mVegS*mVegbmultS + mW*mVegW*mVegbmultW)/4   
  
  #version without nogo areas and vegetation effects
  #mStayers <- (1-pMove)*m  
  #so that flies that would have moved into a neighbouring nogoarea stay
  #if all neighbours are nogo then all flies stay
  # m * (1-pMove*0) = m * 1
  #if no neighbours are no go it collapses to the original above
  # m * (1-pMove*1)
  
  #below is version used in rtMoveRelfectNoGo
  #mStayers <- m * (1- pMove * (mNogN + mNogE + mNogS + mNogW)/4 ) 
  #stayers are influenced by veg in source cell (mVeg) & nogo areas in destination cells (mNogN etc)
  #BEWARE! this is tricky
  #if no neighbouring cells are nogo, all movers move (* (1+1+1+1)/4)
  #if 1 neighbouring cell is nogo, 3/4 movers move (* (0+1+1+1)/4)  
  #if 2 neighbouring cells nogo, 1/2 movers move (* (0+0+1+1)/4)    
  #mStayers <- m * (1- pMove * mVeg * (mNogN + mNogE + mNogS + mNogW)/4 )   

  #adding boundary effects  
  mStayers <- m * (1- (pMove * (mVegbmultNS + mVegbmultEW + mVegbmultSN + mVegbmultWE)/4) * mVeg * (mNogN + mNogE + mNogS + mNogW)/4 )  
  
  #below is not needed now, but might be
  #the num nogo neighbours for every neighbour of this cell
  #   mNumNogNeighbs <- ifelse(mNogW==0,1,0)+
  #                      ifelse(mNogN==0,1,0)+
  #                      ifelse(mNogE==0,1,0)+
  #                      ifelse(mNogS==0,1,0)
  # cat("mNumNogoNeighbs\n") 
  # print(mNumNogoNeighbs)
  #if I wanted to redistribute those that would have gone to a nogo neighbour
  #I would need to count the numNogoNeighbs for the neighbouring cells
  #mArrivers <- pMove*(mW/mNumGoNeighbsW + mN/mNumGoNeighbsN + mE/mNumGoNeighbsE + mS/mNumGoNeighbsS)
  
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
    print(mVeg)
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
    warning("in rtMoveReflectNoGoVegBoundary() num flies seems to have changed during movement, before=",sum(m)," after=",sum(mNew),"\n")
  
  
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



