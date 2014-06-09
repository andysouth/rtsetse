#' a simple spatial tsetse population simulation, a test of phase2
#'
#' \code{rtPhase2Test} runs a simple spatial popn simulation as a test of phase 2
#' model components. Concentrates on movement parameters and mortality so that 
#' it can be used to test popn spread under different popn growth rates.
#' Uses things developed in gridTest.r. Starts with just F.
#' ! check if it works when nRow & nCol == 1 (the aspatial model)

rtPhase2Test <- function( nRow = 10,
                          nCol = 10,
                          pMove = 0.4,
                          iDays = 30,
                          iMaxAge = 120,
                          iCarryCap = 200,
                          iStartAdults = 200,
                          iStartAges = 1,
                          pMortF = 0.05 )
{
  
  ## creating array of [x,y,age]
  dimnames <- list(NULL,NULL,NULL)
  names(dimnames) <- c("x","y","age")
  aF <- array(0, dim=c(nCol,nRow,nAge), dimnames=dimnames)
  
  #adding half of starting adults as females at 5,5
  #aF[5,5,3] <- iStartAdults/2
  #2 params allow number and spread of flies across age classes to be set
  aF[5,5,1:iStartAges] <- iStartAdults/(2*iStartAges)
  
  #also I can easily fill a constant age structure
  #aF[5,5,] <- 100
  #to test the aspatial model
  #aF[1,1,3] <- 100
  
  
  # the most sensible way to save popn record
  # would seem to be to use abind to just add another dimension
  library(abind)
  aRecordF <- abind(aF,along=0) #along=0 binds on new dimension before first
  #! look at keeping names(dimnames(aRecordF))
  #! even with this they get lost later
  names(dimnames(aRecordF)) <- c('day','x','y','age')
  
  for( day in 1:iDays ) {
    
    ##################
    ## adult ageing ##
    #.drop=FALSE makes it work for 1x1 grid
    aF <- aaply(aF, .margins=c(1,2), .drop=FALSE, rtAgeing )
    
    
    ##############
    ## movement ##
    #aF <- aaply(aF, .margins=3, rtMove1 )
    #to pass the pMove arg to rtMove1
    aF <- aaply(aF, .margins=3, function(m) rtMove1(m, pMove=pMove) )    
    #putting array components back in correct order
    aF <- aperm(aF, c(2, 3, 1))
    
    #cat("day",day,"\n")
    
    #aF
    
    #dim(abind(x,y,along=0))     # binds on new dimension before first
    #dim(abind(x,y,rev.along=0)) # binds on new dimension after last
    aRecordF <- abind(aRecordF, aF, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension
    
    
  } #end of iDays loop
  
  
  #showing how records can be accessed
  aRecordF[1,,,3] #grid for day1, age3
#   aRecordF[2,,,4] #grid for day2, age4
#   aRecordF[3,,,5] #grid for day3, age5
#   
#   aRecordF[3,,,] #separate grids for all ages on one day
  aRecordF[3,5,5,] #age structure in one cell on one day  
  
  #returning the popn record
  #! will need to modify later to return M & pupae too
  invisible(aRecordF)
}





