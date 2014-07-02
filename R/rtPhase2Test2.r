#' a simple spatial tsetse population simulation, a 2nd test of phase2
#'
#' \code{rtPhase2Test2} runs a simple spatial popn simulation as a test of phase 2
#' model components. Concentrates on movement parameters and mortality so that 
#' it can be used to test popn spread under different popn growth rates.
#' Uses things developed in gridTest.r. 
#' ! check if it works when nRow & nCol == 1 (the aspatial model)

#' @return a multi-dimensional array [day,x,y,sex,ages]
#' @examples
#' tst <- rtPhase2Test2()
#' @export
#' 
rtPhase2Test2 <- function( 
                          nCol = 10,
                          nRow = 10,
                          pMove = 0.4,
                          iDays = 4,
                          iMaxAge = 7, #120,
                          iCarryCap = 200,
                          iStartAdults = 200,
                          iStartAges = 1,
                          pMortF = 0.05,
                          pMortM = 0.05,
                          loop = TRUE)
{
  
  ##some argument checking
  #if( nRow < 2 | nCol < 2 )
  #  stop("movement does not work if less than 2 grid rows or columns")

  #vectors for death rates for males & females
  #as a first test have mortality rates constant by age
  vpMortF <- rep(pMortF,iMaxAge) 
  vpMortM <- rep(pMortM,iMaxAge) 
  
  #also I can easily fill a constant age structure
  #aF[5,5,] <- 100
  
  #starting to exlplore whether I can have an array with M&F
  #to pass to mortality function
  # 'x','y','sex','age'
  #dimnames <- list(NULL,NULL,NULL,NULL)
  #dimnames <- list(1:nCol,1:nRow,c("F","M"),1:iMaxAge)
  dimnames1 <- list( paste0('x',1:nCol), paste0('y',1:nRow), c("F","M"), paste0('age',1:iMaxAge))
  names(dimnames1) <- c("x","y","sex","age")
  aGrid <- array(0, dim=c(nCol,nRow,2,iMaxAge), dimnames=dimnames1)  
  #this might fill in both M&F
  #adding half of starting adults as each gender to starting cell
  #2 params allow number and distribution of flies across age classes to be set
  aGrid[(nCol+1)/2, (nRow+1)/2, , 1:iStartAges] <- iStartAdults/(2*iStartAges)
  
  #create a matrix for carrying capacity on the grid
  #first test make it constant
  #I could name the dimensions, x & y here
  mCarryCap <- matrix(iCarryCap, ncol=nCol, nrow=nRow)
  
  
  #trying to see how I can access dimensions by name 
#   aGrid['x1','y1','M',] #an age structure for one cell
#   sum(aGrid['x1','y1','M',]) #total M in one cell
#   sum(aGrid['x1','y1',,]) #total pop in one cell
#   aGrid[,,'M','age2'] #a grid of one age  
#   aGrid[,,'F',] #grid of age structures just for F
#   apply(aGrid,MARGIN=c(3,4),sum) #a summed age structures for M&F
#   apply(aGrid,MARGIN=4,sum) #summed age structures for both sexes  
#   apply(aGrid,MARGIN=c(1,2),sum) #grid for all ages & sexes
#   #using names
#   apply(aGrid,MARGIN=c('x','y'),sum) #grid for all ages & sexes
#   apply(aGrid,MARGIN=c('age'),sum) #summed age structure for whole pop
#   apply(aGrid,MARGIN=c('sex'),sum) #summed sex ratio for whole pop  
#   #using apply on subset
  
  
  # the most sensible way to save popn record
  # would seem to be to use abind to just add another dimension
  library(abind)
  aRecord <- abind(aGrid,along=0) #along=0 binds on new dimension before first
  #! look at keeping names(dimnames(aRecordF))
  #! even with this they get lost later
  names(dimnames(aRecord)) <- c('day','x','y','sex','age')
  
  for( day in 1:iDays ) {
    
    ###############
    ## mortality ##
    #this is how it's done in rtPhase1Test
    #!beware of potential bug of passing vectors in wrong order 
    #(remember ladies first) or do it a different way
    #lPop <- rtMortality( vPopF, vPopM, vpMortF, vpMortM, 
    #                     propDD = propMortAdultDD,
    #                     iCarryCap = iCarryCap) 
    #vPopF <- lPop$vFem
    #vPopM <- lPop$vMal  
    
    aGrid <- rtMortalityGrid( aGrid, vpMortF, vpMortM, mCarryCap=mCarryCap, loop=loop )
    
    
    ##################
    ## adult ageing ##
    #!decide whether to call for M&F independently 
    #!or do both in the function
    
#     #passing the whole grid to rtAgeing
#     aF <- rtAgeingGrid(aF)
#     
#     #the third dimension (age) loses it's label
#     #just trying putting it back to see if that solves
#     #"duplicated levels in factors are deprecated"
#     #!this corrected the warnings
#     dimnames(aF) <- list(NULL,NULL,NULL)   
#     names(dimnames(aF)) <- c('x','y','age')
    
    
    
    ##############
    ## movement ##
    #only if >1 row or col
    #! will need to work out how to do it for M&F

    if( nRow > 1 | nCol > 1) {
      
      #aF <- aaply(aF, .margins=3, .drop=FALSE, function(m) rtMove1(m, pMove=pMove) )     
      
      #get F&M separately from aGrid, pass to rtMove1 and put them back in again
      #!could create a loop for 'F' and 'M' here, or do a different way
      aF <- aGrid[,,'F',]
      aF <- aaply(aF, .margins=3, .drop=FALSE, function(m) rtMove1(m, pMove=pMove) ) 
      #!remember always aperm after aaply to put array components back in correct order
      aF <- aperm(aF, c(2,3,1))
      aGrid[,,'F',] <- aF
      
      aM <- aGrid[,,'M',]
      aM <- aaply(aM, .margins=3, .drop=FALSE, function(m) rtMove1(m, pMove=pMove) ) 
      #!remember always aperm after aaply to put array components back in correct order
      aM <- aperm(aM, c(2,3,1))
      aGrid[,,'M',] <- aM      
      
    }
    
    
    cat("day",day,"\n")
    
    #aF
    
    #dim(abind(x,y,along=0))     # binds on new dimension before first
    #dim(abind(x,y,rev.along=0)) # binds on new dimension after last
    aRecord <- abind(aRecord, aGrid, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension
    
    
  } #end of iDays loop
  
  #ensuring that dimnames for the days dimension of aRecord is set
  #note that starts at day0
  dimnames(aRecord)[[1]] <- paste0('day',0:iDays)
  
  #returning the popn record
  #! will need to modify later to return M & pupae too
  invisible(aRecord)
}

#tst <- rtPhase2Test2()
#tst['day0',,,'F','age1']
#tst['day1',,,'F','age1']



