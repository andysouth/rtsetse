#' movement to cells NESW, island model
#' 
#' \code{rtMoveIsland} moves proportion of popn in each cell to the 4 neighbouring cells.
#' Movers are divided equally between the 4 cardinal neighbours.
#' In this initial version popn moving outside of the grid is lost
#' and nothing arrives in from outside of the grid.
#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[x,y,age] to aaply(.margins=3)
#' Tries to cope with nrow or ncol==1 but I think fails.

#' @param m a matrix of cells containing a single number representing one age
#' @param pMove proportion of popn that moves out of the cell.
#' 
#' @return an updated matrix following movement
#' @export

rtMoveIsland <- function(m, pMove=0.4) {
  
  #this puts zeros in, but I could modify it to copy the boundary cell
  #simply by replacing these bits rep(0,nrow(m))
  #that would create reflecting boundaries.
  
  #!beware that this doesn't cope with nrow=1 or ncol=1 
  #in such a case you can probably use c() rather than cbind
  #but it's tricky to work out, R treats vectors and matrices differently
  #m=matrix(1,nrow=1,ncol=3) #to test
  #it's not something we need to do at this stage
#   if( nrow(m) < 2 | ncol(m) < 2 )
#      stop("movement does not work if less than 2 grid rows or columns")
#   
#   mW = cbind( rep(0,nrow(m)), m[,-ncol(m)] )
#   mN = rbind( rep(0,ncol(m)), m[-nrow(m),] )
#   mE = cbind( m[,-1], rep(0,nrow(m)) )
#   mS = rbind( m[-1,], rep(0,ncol(m)) )

  #to cope with nrow=1 or ncol=1 
  #in which case the passed object is likely to be a vector not an array
  #!but not sure I'll be able to tell which dimension?
  #if ( nrow(m) < 2 & ncol(m) < 2 )
  
  #to speed up can just return if there are no popns in matrix
  if ( sum(m)==0 ) return(m)
  
  #trying to get it to cope with nrow=1 or ncol=1  
  mW <- rep(0,nrow(m))
  mE <- rep(0,nrow(m)) 
  #!!BEWARE this is genius but tricky
  #1st case copes with if nrow==1
  #need to treat as vector not matrix
  if ( nrow(m) <2 & ncol(m) > 1 ){
    mW = c( mW, m[,-ncol(m)] )
    mE = c( m[,-1], mE )   
  } else if ( ncol(m) > 1 ){
    mW = cbind( mW, m[,-ncol(m)] )
    mE = cbind( m[,-1], mE )
  } 
  
  mN <- rep(0,ncol(m))
  mS <- rep(0,ncol(m))
  #1st case copes with if ncol==1
  if ( ncol(m) <2 & nrow(m) > 1 ){  
    mN = c( mN, m[-nrow(m),] )
    mS = c( m[-1,], mS )    
  } else if ( nrow(m) > 1 ){
    mN = rbind( mN, m[-nrow(m),] )
    mS = rbind( m[-1,], mS )
  }   
  
  mArrivers <- pMove*(mN + mE + mS + mW)/4
  mStayers <- (1-pMove)*m
  
  mNew <- mArrivers + mStayers
  
  #this avoids duplicate levels problems outside the function
  dimnames(mNew) <- dimnames(m)
  
  return( mNew )
}

#a test on 1D matrix
#!create a unit-test based on this
#m=matrix(c(0,1,0),nrow=1,ncol=3)
#rtMoveIsland(m)
