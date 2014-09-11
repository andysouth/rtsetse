#' tsetse mortality on a grid **in development**
#'
#' \code{rtMortalityGrid} returns an array with the age distributions of males & females [x,y,sex,age]
#' \cr It accepts age and sex specific mortality probabilities.
#' \cr !It should check that the length of the age structure and pMort vectors are the same
#' \cr It uses the length of the age structure vectors passed to it.
#' \cr There could be an option to do F only if only F are passed.

#' @param aGrid an array with the age distributions of males & females [x,y,sex,age] 
#' @param vpMortF a vector of age-specific mortality probabilities of Females 
#' @param vpMortM a vector of age-specific mortality probabilities of Males 
#' @param propDD proportion of mortality that is density dependent 
#' @param mCarryCap a matrix of Carrying Capacities for each cell as an integer
#' @param loop TEMPORARY var to test a loop vs apply solution, only loop=TRUE works fully
#' 
#' @return an array with the age distributions of males & females [x,y,sex,age]
#' @export

rtMortalityGrid <- function( aGrid,
                         vpMortF,
                         vpMortM,
                         propDD = 0.25,
                         mCarryCap = NA,
                         loop = TRUE ) 
{
 
 
  #aGrid[x,y,sex,age]
  #mort differs by sex & age

  #there is a loop solution and an unfinished solution avoiding loops

  #tests from 16/6/14 showed the loop solution is a tiny bit slower. 
  #But I think it may be worth it, it’s much simpler and I haven’t even 
  #quite worked out how to implement the non-loop solution yet.
  
  if (loop){

    #LOOP solution
    #to implement DD on a grid 

    #calls existing rtMortality() function (modified to return an array)
    
    for(x in seq_along(dimnames(aGrid)$x)){
      for(y in seq_along(dimnames(aGrid)$y)){
        
        #cat(paste("x,y:",x,",",y,"dim(mCarryCap)=",dim(mCarryCap),"\n"))
        
        #!!!annoying issue about confusing dimensions
        #!!!i changed int around just to get it to work
        
        #get carry cap from the matrix
        #iCarryCap <- mCarryCap[x,y]  
        iCarryCap <- mCarryCap[y,x]  
        
        
        aGrid[x,y,,] <- rtMortality(vFem = aGrid[x,y,'F',],
                                    vMal = aGrid[x,y,'M',],
                                    vpMortF,
                                    vpMortM,
                                    returnArray = TRUE,
                                    propDD = propDD, 
                                    iCarryCap = iCarryCap )
      }#y
    }#x


  } else {
    
    
    #create a low level mort function (similar to what I started with!)
    rtMort <- function(vAges,vpMortByAge){
      #trying as a loop just to get started
      for( age in seq_along(vAges) )  {
        vAges[age] <- vAges[age] * (1-vpMortByAge[age])   
      } 
      #return
      vAges
    }    
    
    #this calls the mortality function for both sexes
    #tst <- apply( aGrid,MARGIN=c('x','y','sex'), rtMort, vpMortByAge=vpMortF )
    
    #to apply mort to sexes separately (for each cell in the grid)
    #F
    aF <- aGrid[,,'F',]
    aF <- apply( aF, MARGIN=c('x','y'), rtMort, vpMortByAge=vpMortF )
    #do I need to aperm ? yes
    aF <- aperm(aF, c(2,3,1))
    #put aF back into grid
    aGrid[,,'F',] <- aF
    #M
    aM <- aGrid[,,'M',]
    aM <- apply( aM, MARGIN=c('x','y'), rtMort, vpMortByAge=vpMortM )
    #do I need to aperm ? yes
    aM <- aperm(aM, c(2,3,1))
    #put aF back into grid
    aGrid[,,'M',] <- aM
    
  }#end of unfinished non loop solution


  #returning a modified array
  invisible( aGrid )
  
} 