#' movement to cells NESW
#' 
#' \code{rtMove1} moves proportion of popn in each cell to the 4 neighbouring cells.
#' Movers are divided equally between the 4 cardinal neighbours.
#' In this initial version popn moving outside of the grid is lost
#' and nothing arrives in from outside of the grid.
#' This function works on a single age class, it can be made to work on multiple age classes
#' by passing an array[x,y,age] to aaply(.margins=3)

#' @param m a matrix of cells containing a single number representing one age
#' @param pMove proportion of popn that moves out of the cell.
#' 
#' @return an updated matrix following movement
#' @export

rtMove1 <- function(m, pMove=0.4) {
  
  #this puts zeros in, but I could modify it to copy the boundary cell
  #simply by replacing these bits rep(0,nrow(m))
  #that would create reflecting boundaries.
  mW = cbind( rep(0,nrow(m)), m[,-nrow(m)] )
  mN = rbind( rep(0,ncol(m)), m[-ncol(m),] )
  mE = cbind( m[,-1], rep(0,nrow(m)) )
  mS = rbind( m[-1,], rep(0,ncol(m)) )
  
  mArrivers <- pMove*(mN + mE + mS + mW)/4
  mStayers <- (1-pMove)*m
  
  mNew <- mArrivers + mStayers
  
  #this avoids duplicate levels problems outside the function
  dimnames(mNew) <- dimnames(m)
  
  return( mNew )
}