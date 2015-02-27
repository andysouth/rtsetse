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
#' @param mnog a matrix of cells of 0&1, 0 for nogo areas 
#' @param mveg a matrix of vegetation movement modifiers >1 increases movement out of the cell, <1 decreases movement out of the cell 
#' @param pMove proportion of popn that moves out of the cell.
#' @param verbose print what it's doing T/F
#' 
#' @return an updated matrix following movement
#' @examples
#' #1 nogo neighbour
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mnog = array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #2 nogo neighbours
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mnog = array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #3 nogo neighbours
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mnog = array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' #4 nogo neighbours, all flies stay
#' rtMoveReflectNoGoVegBoundary(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)), mnog = array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4)), verbose=TRUE)
#' @export

rtMoveReflectNoGoVegBoundary <- function(m = array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)),
                                 mnog = NULL,
                                 mveg = NULL,
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
  if (!is.null(mnog))
  {
    mnogN = rbind( mnog[1,], mnog[-nrow(mnog),] )
    mnogE = cbind( mnog[,-1], mnog[,ncol(mnog)] )
    mnogS = rbind( mnog[-1,], mnog[nrow(mnog),] )   
    mnogW = cbind( mnog[,1], mnog[,-ncol(mnog)] )    
  } else 
  {
    #set all these to 1 so they have no effect on movement calc later
    mnog <- mnogN <- mnogE <- mnogS <- mnogW <- 1
  }
  
  
  #vegetation movement modifiers from source cells
  
  if (!is.null(mveg))
  {
    mvegN = rbind( mveg[1,], mveg[-nrow(mveg),] )
    mvegE = cbind( mveg[,-1], mveg[,ncol(mveg)] )
    mvegS = rbind( mveg[-1,], mveg[nrow(mveg),] )   
    mvegW = cbind( mveg[,1], mveg[,-ncol(mveg)] )    
  } else 
  {
    #set all these to 1 so they have no effect on movement calc later
    mveg <- mvegN <- mvegE <- mvegS <- mvegW <- 1
  }
  
  
  #check for if any cells in pMove*mveg are >1
  #if so set to 1 so that all indivs leave
  indicesHighMove <- which((mveg*pMove > 1))
  if (length(indicesHighMove) >0)
  {
    warning("your combination of pMove and vegetation movement multipliers causes ",length(indicesHighMove),
            " cells to have proportion moving >1, these will be set to 1 and all will move out")
    #reduce multiplier in cells so that the result will be 1 (all move)
    mveg[indicesHighMove] <- 1/pMove
  }
  
  
  ######################################
  #todo get veg boundary effects working
  #add a decrease in movement from 'better' to 'poorer' vegetation types as in Hat-trick
  #I need to calculate the change associated with each move
  #e.g. mvegN-mveg for source-destination
  #?can I calculate once based on the vegmap and save results as an array ?
  #avegchange[y,x,(N,E,S,W)]
  
  #in Hat-trick there is a categorical decrease, from Hat-trick doc : 
  #if destination is less like best veg than source, movement decreased by about 1/3 for every category away from the nearest one with a 100% entry
  #1 category, 30%
  #2 categories, 10%
  #3 categories, 3%
  #4 categories, 1%
  #5 categories, 0.3%
  #This relies on there being a sequence of vegetation density
  #I can try to get it to work for savannah first
  #I can convert veg category map to density sequence : 1,2,3,4,5 ..
  #Have an input of the best veg type (e.g. 3)
  #then algorithm is just :
  #qualityChange = abs(from-best) - abs(to-best)
  #if (qualityChange) >= 0 move=100%
  #else if (qualityChange < 0) decrease movement
  #I might even be able generalise it and allow movement to be increased
  #or maybe not, the maximum is that all movers go and can't increase on that easily
  #can pass a vector of boundaryMovementModifiers from 0 to maxChange
  #default for Hat-trick is
  #1, 0.3, 0.1, 0.03, 0.01, 0.001 
  #I could write an option into readMapVeg to return numeric values
  #todo get this to work on a simpler version
  #mvegCats <- rtReadMapVeg( system.file("extdata","vegTanzaniaSerengetiTorr1km.txt", package="rtsetse"))
  
  #test vegCats
  mvegCats <- array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4))
  
  #for now can just convert to difPref
  mvegNum <- rtSetGridFromVeg( mvegCats, dfLookup=data.frame(from=c("D","T","O","S","B","G","N"),to=c(1,2,3,4,5,6,999),stringsAsFactors = FALSE ))
  iBest <- 4 
  #matrix of difference of veg in cell from best
  mvegDifPref <- abs(iBest-mvegNum)
  
  #remember
  #in the code below matrices with NESW on end are source cells
  #matrices without are destination cells
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #so I need to create NESW matrices that represent the change in veg preference associated with that move
  #first create copies
  mvegDifPrefN <- rbind( mvegDifPref[1,], mvegDifPref[-nrow(mvegDifPref),] )
  mvegDifPrefE <- cbind( mvegDifPref[,-1], mvegDifPref[,ncol(mvegDifPref)] )
  mvegDifPrefS <- rbind( mvegDifPref[-1,], mvegDifPref[nrow(mvegDifPref),] )   
  mvegDifPrefW <- cbind( mvegDifPref[,1], mvegDifPref[,-ncol(mvegDifPref)] )      
  #then do calculation, source-destination
  mvegDifPrefN <- mvegDifPrefN - mvegDifPref
  mvegDifPrefE <- mvegDifPrefE - mvegDifPref
  mvegDifPrefS <- mvegDifPrefS - mvegDifPref
  mvegDifPrefW <- mvegDifPrefW - mvegDifPref
  #seems to work
  #unique(as.vector(mvegDifPrefN))
  #now convert the change associated with the move to a modifier of the movement rate
  #convert 0:5 to   1, 0.3, 0.1, 0.03, 0.01, 0.001 
  #mvegbmult mveg boundary multiplier
  #todo write dryer version of this & previous similar actions
  mvegbmultN <- rtSetGridFromVeg( mvegDifPrefN, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mvegbmultE <- rtSetGridFromVeg( mvegDifPrefE, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mvegbmultS <- rtSetGridFromVeg( mvegDifPrefS, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  mvegbmultW <- rtSetGridFromVeg( mvegDifPrefW, dfLookup=data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1)))
  
  #******HERE
  #these go the opposite way
#   mvegbmultSN <- rbind( mvegbmultS[1,], mvegbmultS[-nrow(mvegbmultN),] )
#   mvegbmultWE <- cbind( mvegbmultW[,-1], mvegbmultW[,ncol(mvegbmultE)] )
#   mvegbmultNS <- rbind( mvegbmultN[-1,], mvegbmultN[nrow(mvegbmultS),] )   
#   mvegbmultEW <- cbind( mvegbmultE[,1], mvegbmultE[,-ncol(mvegbmultW)] )    

  mvegbmultSN <- rbind( rep(1,ncol(m)), mvegbmultS[-nrow(mvegbmultN),] )
  mvegbmultWE <- cbind( mvegbmultW[,-1], rep(1,nrow(m)) )
  mvegbmultNS <- rbind( mvegbmultN[-1,], rep(1,ncol(m)) )   
  mvegbmultEW <- cbind( rep(1,nrow(m)), mvegbmultE[,-ncol(mvegbmultW)] )      
  #mN = rbind( rep(0,ncol(m)), m[-nrow(m),] )
  #mE = cbind( m[,-1], rep(0,nrow(m)) )
  #mS = rbind( m[-1,], rep(0,ncol(m)) )
  #mW = cbind( rep(0,nrow(m)), m[,-ncol(m)] )
  
                                   
  #calc arrivers in a cell from it's 4 neighbours
  #mArrivers <- pMove*(mN + mE + mS + mW)/4
  
  #add that movers aren't received at a cell if it is nogo
  #below is version used in rtMoveRelfectNoGo
  #mArrivers <- pMove*(mN*mnog + mE*mnog + mS*mnog + mW*mnog)/4  
  
  #mnog at the destination cell that matters, and mveg at the source cell
  #uses mvegN & mN etc for source cells, mnog for the destination cells 
  #mArrivers <- pMove*(mN*mvegN*mnog + mE*mvegE*mnog + mS*mvegS*mnog + mW*mvegW*mnog)/4 
  #this is equivalent to above and simpler
  #mArrivers <- pMove*mnog*(mN*mvegN + mE*mvegE + mS*mvegS + mW*mvegW)/4    
  #adding boundary effects
  mArrivers <- pMove*mnog*(mN*mvegN*mvegbmultN + mE*mvegE*mvegbmultE + mS*mvegS*mvegbmultS + mW*mvegW*mvegbmultW)/4   
  
  #version without nogo areas and vegetation effects
  #mStayers <- (1-pMove)*m  
  #so that flies that would have moved into a neighbouring nogoarea stay
  #if all neighbours are nogo then all flies stay
  # m * (1-pMove*0) = m * 1
  #if no neighbours are no go it collapses to the original above
  # m * (1-pMove*1)
  
  #below is version used in rtMoveRelfectNoGo
  #mStayers <- m * (1- pMove * (mnogN + mnogE + mnogS + mnogW)/4 ) 
  #stayers are influenced by veg in source cell (mveg) & nogo areas in destination cells (mnogN etc)
  #BEWARE! this is tricky
  #if no neighbouring cells are nogo, all movers move (* (1+1+1+1)/4)
  #if 1 neighbouring cell is nogo, 3/4 movers move (* (0+1+1+1)/4)  
  #if 2 neighbouring cells nogo, 1/2 movers move (* (0+0+1+1)/4)    
  #mStayers <- m * (1- pMove * mveg * (mnogN + mnogE + mnogS + mnogW)/4 )   

  #adding boundary effects  
  mStayers <- m * (1- (pMove * (mvegbmultNS + mvegbmultEW + mvegbmultSN + mvegbmultWE)/4) * mveg * (mnogN + mnogE + mnogS + mnogW)/4 )  
  
  #below is not needed now, but might be
  #the num nogo neighbours for every neighbour of this cell
  #   mNumNogNeighbs <- ifelse(mnogW==0,1,0)+
  #                      ifelse(mnogN==0,1,0)+
  #                      ifelse(mnogE==0,1,0)+
  #                      ifelse(mnogS==0,1,0)
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
  
  # cat("\nmnog\n") 
  # print(mnog)
  
  if (verbose)
  {
    cat("popn before\n") 
    print(m)
    cat("\nno-go areas (0=nogo)\n") 
    print(mnog)
    cat("\nveg movement multiplier\n") 
    print(mveg)
    cat("\nveg dif from preferred\n") 
    print(mvegDifPref)
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


