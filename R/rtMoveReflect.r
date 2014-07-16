#' movement to cells NESW, reflecting boundaries
#' 
#' \code{rtMoveReflect} moves proportion of popn in each cell to the 4 neighbouring cells.
#' Movers are divided equally between the 4 cardinal neighbours.
#' Boundaries are reflecting.
#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[x,y,age] to aaply(.margins=3)
#' Doesn't try to cope with nrow or ncol==1.

#' @param m a matrix of cells containing a single number representing one age
#' @param pMove proportion of popn that moves out of the cell.
#' 
#' @return an updated matrix following movement
#' @export

rtMoveReflect <- function(m, pMove=0.4) {
  
  #this puts zeros in, but I could modify it to copy the boundary cell
  #simply by replacing these bits rep(0,nrow(m))
  #that would create reflecting boundaries.
  
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
  #mW = cbind( rep(0,nrow(m)), m[,-ncol(m)] )
  #mN = rbind( rep(0,ncol(m)), m[-nrow(m),] )
  #mE = cbind( m[,-1], rep(0,nrow(m)) )
  #mS = rbind( m[-1,], rep(0,ncol(m)) )
  #reflecting boundaries
  #0's from island model above are replaced with a copy of boundary row or col
  mW = cbind( m[,1], m[,-ncol(m)] )
  mN = rbind( m[1,], m[-nrow(m),] )
  mE = cbind( m[,-1], m[,ncol(m)] )
  mS = rbind( m[-1,], m[nrow(m),] ) 
  
  #calc arrivers in a cell from it's 4 neighbours
  mArrivers <- pMove*(mN + mE + mS + mW)/4
  mStayers <- (1-pMove)*m
  
  mNew <- mArrivers + mStayers
  
  #this avoids duplicate levels problems outside the function
  dimnames(mNew) <- dimnames(m)
  
  return( mNew )
}

#simple test
#!create a unit-test based on this
#m=matrix(c(0,1,0),nrow=3,ncol=3)
#m=matrix(c(1:9),nrow=3,ncol=3)
#rtMoveReflect(m)
