#' movement affected by vegetation and NoGo areas, to cells NESW, reflecting boundaries
#' 
#' \code{rtMoveReflectNoGoVeg} moves proportion of popn in each cell to the 4 neighbouring cells.
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
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' #1 nogo neighbour
#' rtMoveReflectNoGoVeg(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #2 nogo neighbours
#' rtMoveReflectNoGoVeg(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #3 nogo neighbours
#' rtMoveReflectNoGoVeg(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #4 nogo neighbours, all flies stay
#' rtMoveReflectNoGoVeg(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' @export

rtMoveReflectNoGoVeg <- function(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)),
                                 mNog = NULL,
                                 mVeg = NULL,
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
  #island model uses 0's
  #mN = rbind( rep(0,ncol(m)), m[-nrow(m),] )
  #mE = cbind( m[,-1], rep(0,nrow(m)) )
  #mS = rbind( m[-1,], rep(0,ncol(m)) )
  #mW = cbind( rep(0,nrow(m)), m[,-ncol(m)] )
  #reflecting boundaries
  #0's from island model above are replaced with a copy of boundary row or col
  mN = rbind( m[1,], m[-nrow(m),] )
  mE = cbind( m[,-1], m[,ncol(m)] )
  mS = rbind( m[-1,], m[nrow(m),] ) 
  mW = cbind( m[,1], m[,-ncol(m)] )  
  
  #creating matrices of neighbouring nogo areas
  #this doesn't need to be repeated every day
  #it could be done at the start of a simulation, and passed probably as a list or array
  #but time cost of doing this for a few 100 days is probably fairly low
  if (!is.null(mNog))
  {
    mNogN = rbind( mNog[1,], mNog[-nrow(mNog),] )
    mNogE = cbind( mNog[,-1], mNog[,ncol(mNog)] )
    mNogS = rbind( mNog[-1,], mNog[nrow(mNog),] )   
    mNogW = cbind( mNog[,1], mNog[,-ncol(mNog)] )    
  } else 
  {
    #set all these to 1 so they have no effect on movement calc later
    mNog <- mNogN <- mNogE <- mNogS <- mNogW <- 1
  }

 
  #vegetation movement modifiers from source cells

  if (!is.null(mVeg))
  {
    mVegN = rbind( mVeg[1,], mVeg[-nrow(mVeg),] )
    mVegE = cbind( mVeg[,-1], mVeg[,ncol(mVeg)] )
    mVegS = rbind( mVeg[-1,], mVeg[nrow(mVeg),] )   
    mVegW = cbind( mVeg[,1], mVeg[,-ncol(mVeg)] )    
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
  
  #calc arrivers in a cell from it's 4 neighbours
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  
  #add that movers aren't received at a cell if it is nogo
  #below is version used in rtMoveRelfectNoGo
  #mArrivers <- pMove*(mN*mNog + mE*mNog + mS*mNog + mW*mNog)/4  

  #mNog at the destination cell that matters, and mVeg at the source cell
  #uses mVegN & mN etc for source cells, mNog for the destination cells 
  #mArrivers <- pMove*(mN*mVegN*mNog + mE*mVegE*mNog + mS*mVegS*mNog + mW*mVegW*mNog)/4 
  #this is equivalent to above and simpler
  mArrivers <- pMove*mNog*(mN*mVegN + mE*mVegE + mS*mVegS + mW*mVegW)/4    
  
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
  mStayers <- m * (1- pMove * mVeg * (mNogN + mNogE + mNogS + mNogW)/4 )   
  

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
    cat("\nmStayers\n") 
    print(mStayers)
    cat("\nmArrivers\n") 
    print(mArrivers)
    cat("\nmNew\n") 
    print(mNew)
  }

  #one way of testing this is that the total number of flies shouldn't have changed
  #(i think reflecting edges mean should get same in as out)
  #float rounding cause small differences, this checks for differences >1 fly
  if (sum(m) - sum(mNew) > 1)
    warning("in rtMoveReflectNoGoVeg() num flies seems to have changed during movement, before=",sum(m)," after=",sum(mNew),"\n")
    

  invisible( mNew )
}


