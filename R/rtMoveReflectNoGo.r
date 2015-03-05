#' movement with NoGo areas, to cells NESW, reflecting boundaries
#' 
#' \code{rtMoveReflectNoGo} moves proportion of popn in each cell to the 4 neighbouring cells.
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
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' #1 nogo neighbour
#' rtMoveReflectNoGo(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #2 nogo neighbours
#' rtMoveReflectNoGo(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #3 nogo neighbours
#' rtMoveReflectNoGo(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #4 nogo neighbours, all flies stay
#' rtMoveReflectNoGo(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mNog = array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' @export

rtMoveReflectNoGo <- function(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)),
                          mNog = array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4)),
                          pMove=0.4,
                          verbose=FALSE) {
  
  #cat("in rtMoveReflectNoGo\n")
  
  #!beware that this doesn't cope with nrow=1 or ncol=1 
  #see rtMoveIsland() which tries (and i think fails) to sort 
  #tricky to work out, R treats vectors and matrices differently
  
  if( nrow(m) < 2 | ncol(m) < 2 )
    stop("reflecting movement does not work if less than 2 grid rows or columns")
  
  #to speed up can just return if there are no popns in matrix
  if ( sum(m)==0 ) return(m)
  
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
  mNogN = rbind( mNog[1,], mNog[-nrow(mNog),] )
  mNogE = cbind( mNog[,-1], mNog[,ncol(mNog)] )
  mNogS = rbind( mNog[-1,], mNog[nrow(mNog),] )   
  mNogW = cbind( mNog[,1], mNog[,-ncol(mNog)] )
  
  #calc arrivers in a cell from it's 4 neighbours
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  #so that neighbouring nogo areas don't provide arrivers to this cell
  #mArrivers <- pMove*(mN*mNogN + mE*mNogE + mS*mNogS + mW*mNogW)/4
  
  mArrivers <- pMove*(mN*mNog + mE*mNog + mS*mNog + mW*mNog)/4  
  
  #mStayers <- (1-pMove)*m  
  #so that flies that would have moved into a neighbouring nogoarea stay
  #if all neighbours are nogo then all flies stay
  # m * (1-pMove*0) = m * 1
  #if no neighbours are no go it collapses to the original above
  # m * (1-pMove*1)

  mStayers <- m * (1- pMove * (mNogN + mNogE + mNogS + mNogW)/4 )   
  

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
    warning("in rtMoveReflectNoGo() num flies seems to have changed during movement, before=",sum(m)," after=",sum(mNew),"\n")
    

  invisible( mNew )
}

#difficulties
#how do movers get into the bottom corner in this e.g. (might be to do with reflecting boundaries)
#rtMoveReflectNoGo(m = matrix(c(0,1,0,0,1,0,0,1,0),nrow=3), mNog = matrix(c(1,0,1,0,1,0,1,0,1),nrow=3), verbose=TRUE)

#this works well and shows no movement to the bottom row
#rtMoveReflectNoGo(m = matrix(c(0,1,0,0,1,0,0,1,0),nrow=3), mNog = matrix(c(0,1,0,0,1,0,0,1,0),nrow=3), verbose=TRUE)
#this works too
#rtMoveReflectNoGo(m = matrix(c(0,1,0,0,1,0,0,1,0),nrow=3), mNog = matrix(c(1,1,0,1,1,0,1,1,0),nrow=3), verbose=TRUE)


#simple test
#!create a unit-test based on this
#m=matrix(c(0,1,0),nrow=3,ncol=3)
#m=matrix(c(1:9),nrow=3,ncol=3)
#rtMoveReflect(m)
