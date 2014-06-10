#' a simple spatial tsetse population simulation, a test of phase2
#'
#' \code{rtPhase2Test} runs a simple spatial popn simulation as a test of phase 2
#' model components. Concentrates on movement parameters and mortality so that 
#' it can be used to test popn spread under different popn growth rates.
#' Uses things developed in gridTest.r. Starts with just F.
#' ! check if it works when nRow & nCol == 1 (the aspatial model)

#' @return a multi-dimensional array [day,x,y,ages] just for F to start
#' @examples
#' tst <- rtPhase2Test()
#' @export
#' 
rtPhase2Test <- function( nRow = 10,
                          nCol = 10,
                          pMove = 0.4,
                          iDays = 8,
                          iMaxAge = 120,
                          iCarryCap = 200,
                          iStartAdults = 200,
                          iStartAges = 1,
                          pMortF = 0.05 )
{
  
  ##some argument checking
  #if( nRow < 2 | nCol < 2 )
  #  stop("movement does not work if less than 2 grid rows or columns")
  
  ## creating array of [x,y,age]
  #can fill dimnames but not necessary, indeed can cause duplicated levels warnings
  dimnames <- list(NULL,NULL,NULL)
  names(dimnames) <- c("x","y","age")
  aF <- array(0, dim=c(nCol,nRow,iMaxAge), dimnames=dimnames)
  
  #adding half of starting adults as females at 5,5
  #aF[5,5,3] <- iStartAdults/2
  #2 params allow number and spread of flies across age classes to be set
  aF[(nCol+1)/2, (nRow+1)/2, 1:iStartAges] <- iStartAdults/(2*iStartAges)
  
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
    #the third dimension (age) loses it's label
    #just trying putting it back to see if that solves
    #"duplicated levels in factors are deprecated"
    names(dimnames(aF)) <- c('x','y','age')
    #trying to empty the dimnames themselves
    #!this corrected the warnings
    dimnames(aF) <- list(NULL,NULL,NULL)
    
    
    ##############
    ## movement ##
    #only if >1 row or col
    if( nRow > 1 | nCol > 1) {
      #aF <- aaply(aF, .margins=3, rtMove1 )
      #to pass the pMove arg to rtMove1
      aF <- aaply(aF, .margins=3, .drop=FALSE, function(m) rtMove1(m, pMove=pMove) )    
      #putting array components back in correct order
      aF <- aperm(aF, c(2, 3, 1))     
    }

    
    cat("day",day,"\n")
    
    #aF
    
    #dim(abind(x,y,along=0))     # binds on new dimension before first
    #dim(abind(x,y,rev.along=0)) # binds on new dimension after last
    aRecordF <- abind(aRecordF, aF, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension
    
    
  } #end of iDays loop
  
  
  #showing how records can be accessed
#  aRecordF[1,,,3] #grid for day1, age3
#   aRecordF[2,,,4] #grid for day2, age4
#   aRecordF[3,,,5] #grid for day3, age5
#   
#   aRecordF[3,,,] #separate grids for all ages on one day
#  aRecordF[3,5,5,] #age structure in one cell on one day  
  
#   #to sum the age structures in each cell on each day
#   #and give result as [days,x,y]
#   tst2 <- aaply( tst, .margins=c(1,2,3), sum )
# 
#   #to create a raster from day 1
#   rst1 <- raster(tst2[1,,])
#   #trying to create a list of rasters
#   #as a step towards a rasterStack
#   #listRast <- aaply( tst2, .margins=1, .drop=FALSE, raster)
#   #but this gives a list of 500 = days*x*y rather than days
#   #seems to be an interaction with raster
#   #because this does create a vector of [days]
#   #listRast <- aaply( tst2, .margins=1, sum)
# 
#   #seems that there is a brick method for using arrays
#   #and that RasterBrick may be preferable to Stack when data are in memory
#   #it assumes x,y,z order so I need to use aperm
#   tst2 <- aperm(tst2, c(2, 3, 1))
#   brick1 <- brick(tst2) 
#   plot(brick1)  
# 
#   #to get a map of the final day
#   #without doing the summarising for all days
#   iFinalDay <- dim(aRecordF)[1]
#   aFinalDay <- aRecordF[iFinalDay,,,]
#   #to sum the age structures in each cell on each day
#   #and give result as [days,x,y]
#   mDays <- aaply( aFinalDay, .margins=c(1,2), sum )
#   
#   rast1 <- raster(mDays)
#   plot(rast1)


  #returning the popn record
  #! will need to modify later to return M & pupae too
  invisible(aRecordF)
}

# #trying to fix problem of droppped dimensions when nrow=1 or ncol=1
# tst <- rtPhase2Test(nRow=1,iDays=2)
# dim(tst)
# #[1]   3  10   1 120
# aDays <- aaply( tst, .margins=c(1,2,3), sum )
# #aDays <- aaply( tst, .margins=c(1,2,3), .drop=FALSE, sum )
# #> dim(aDays)
# #[1]  3 10
# #which is days,cols
# #perhaps I can add row dimension of 1 back on ?
# #I want it to be 3,10,1
# #aha!
# aDays2 <- aDays
# dim(aDays2) <- c(3,10,1)

#rearranging dimensions for raster brick
# aDays3 <- aperm(aDays2, c(2, 3, 1))



