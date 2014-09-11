#' a simple spatial tsetse population simulation, a 3rd test of phase2
#'
#' \code{rtPhase2Test3} goes back to more hat-trick like density dependence.
#' runs a simple spatial popn simulation as a test of phase 2
#' model components. Concentrates on movement parameters and mortality so that 
#' it can be used to test popn spread under different popn growth rates.
#' ! check if it works when nRow & nCol == 1 (the aspatial model)


#' @param nCol number grid columns
#' @param nRow number grid rows
#' @param pMove probability of moving between cells
# following are same as rtPhase1Test3
#' @param iDays days to run simulation
#' @param iMaxAge max age of fly allowed in model (will warn if flies age past this)
#' @param iCarryCapF carrying capacity of adult females 
#' @param fMperF numbers of males per female, default 0.5 for half as many M as F
#' @param iStartAdults number of adults to start simulation with
#' @param iStartAges spread start adults across the first n ages classes
#' @param iStartPupae number of pupae to start simulation with (they get spread across sex&age)
#'     option "sameAsAdults" to set tot pupae same as tot adults.
#' @param pMortF adult female mortality on day1, rates on later days are determined by following parameters.
#' @param pMortM adult male mortality on day1, rates on later days are determined by following parameters.
#' @param iMortMinAgeStart  Age at which min death rates start. 
#' @param iMortMinAgeStop   Age at which min death rates stop.
#' @param fMortMinProp  What proportion of the maximum death rate on day 0 is the minimum death rate.
#' @param fMortOldProp  What proportion of the maximum death rate on day 0 is the death rate after iDeathMinAgeStop.
#' @param propMortAdultDD proportion of adult mortality that is density dependent
#' @param pMortPupa pupal mortality per period
#' @param propMortPupaDD proportion of pupal mortality that is density dependent
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(M) to develop
#' @param iFirstLarva Age that female produces first larva
#' @param iInterLarva Inter-larval period
#' @param pMortLarva larval mortality per period
#' @param propMortLarvaDD proportion of larval mortality that is density dependent
#' 
#' @param report filename for a report for this run, if not specified no report is produced

#' @return a multi-dimensional array [day,x,y,sex,ages]
#' @examples
#' \dontrun{
#' tst <- rtPhase2Test3()
#' rtPlotMapPop(tst)
#' }
#' @export
#' 
rtPhase2Test3 <- function( 
                          nCol = 10,
                          nRow = 10,
                          pMove = 0.4,
                          iDays = 4,
                          iMaxAge = 120,
                          iCarryCapF = 200,
                          fMperF = 0.5,
                          iStartAdults = 200,
                          iStartAges = 1,
                          iStartPupae = "sameAsAdults",
                          pMortF = 0.05,
                          pMortM = 0.05,
                          iMortMinAgeStart = 10,
                          iMortMinAgeStop = 50,
                          fMortMinProp = 0.2,
                          fMortOldProp = 0.3,
                          propMortAdultDD = 0.25,
                          pMortPupa = 0.25,
                          propMortPupaDD = 0.25,
                          iPupDurF = 26,
                          iPupDurM = 28,
                          iFirstLarva = 16,
                          iInterLarva = 10,
                          pMortLarva = 0.05,
                          propMortLarvaDD = 0.25,
                          report = NULL ) #"reportPhase2.html" ) 
{
  
  ##some argument checking
  #if( nRow < 2 | nCol < 2 )
  #  stop("movement does not work if less than 2 grid rows or columns")

  #testing getting the arguments
  #callObject <- match.call() only returns specified args
  #callObject <- call() Error 'name' is missing  
  #named_args <- as.list(parent.frame()) #does something weird, just gives the output object
  lNamedArgs <- mget(names(formals()),sys.frame(sys.nframe()))
  
  
  #setting a total carryCap from the female input
  #used later e.g. in mortality
  iCarryCap <- iCarryCapF * 1+fMperF
  
  #vectors for death rates for males & females
  #as a first test have mortality rates constant by age
  vpMortF <- rep(pMortF,iMaxAge) 
  vpMortM <- rep(pMortM,iMaxAge) 
  
    
  #ADULTS
  # 'x','y','sex','age'
  #dimnames <- list(1:nCol,1:nRow,c("F","M"),1:iMaxAge)
  dimnames1 <- list( paste0('x',1:nCol), paste0('y',1:nRow), c("F","M"), paste0('age',1:iMaxAge))
  names(dimnames1) <- c("x","y","sex","age")
  aGrid <- array(0, dim=c(nCol,nRow,2,iMaxAge), dimnames=dimnames1)  
  #adding half of starting adults as each gender to starting cell
  #2 params allow number and distribution of flies across age classes to be set
  aGrid[(nCol+1)/2, (nRow+1)/2, , 1:iStartAges] <- iStartAdults/(2*iStartAges)
  
  #aF[5,5,] <- 100 #could be used easily to fill a constant age structure
  
  #PUPAE 
  #similar array to adults except numAges is different
  #and can potentially be different betwenn M&F
  iMaxPupAge <- max(iPupDurM, iPupDurF)
  dimnamesPup <- list( paste0('x',1:nCol), paste0('y',1:nRow), c("F","M"), paste0('age',1:iMaxPupAge))
  names(dimnamesPup) <- c("x","y","sex","age")
  aGridPup <- array(0, dim=c(nCol,nRow,2,iMaxPupAge), dimnames=dimnamesPup)  
  
  #option to set tot pupae same as tot adults
  if (iStartPupae=="sameAsAdults")
    iStartPupae <- iStartAdults
  
  #pupae are done slightly differently from adults
  #it puts indivs into all age classes
  #because males stay in the ground longer this means there will be more males
  #next 3 lines same code as from rtPhase1Test2.r
  fPupaPerSexAge <- iStartPupae/(iPupDurF+iPupDurM)
  vPupaM <- rep(fPupaPerSexAge, iPupDurM)
  #make the F vector up to the same length as the M with extra 0's
  #vPupaF <- rep(fPupaPerSexAge, iPupDurF)
  vPupaF <- c(rep(fPupaPerSexAge, iPupDurF),rep(0,iPupDurM-iPupDurF))
  #then put each pupal vector into the array
  aGridPup[(nCol+1)/2, (nRow+1)/2,'F', ] <- vPupaF
  aGridPup[(nCol+1)/2, (nRow+1)/2,'M', ] <- vPupaM
  
  
  
  #create a matrix for carrying capacity on the grid
  #first test make it constant
  #I could name the dimensions, x & y here
  mCarryCap <- matrix(iCarryCap, ncol=nCol, nrow=nRow)
  
  
# access array dimensions by name 
#   aGrid['x1','y1','M',] #an age structure for one cell
#   sum(aGrid['x1','y1','M',]) #total M in one cell
#   sum(aGrid['x1','y1',,]) #total pop in one cell
#   aGrid[,,'M','age2'] #a grid of one age  
#   aGrid[,,'F',] #grid of age structures just for F
#   apply(aGrid,MARGIN=c('x','y'),sum) #grid for all ages & sexes
#   apply(aGrid,MARGIN=c('age'),sum) #summed age structure for whole pop
#   apply(aGrid,MARGIN=c('sex'),sum) #summed sex ratio for whole pop  
#   #using apply on subset
  
  
  # the most sensible way to save popn record
  # would seem to be to use abind to just add another dimension
  #library(abind)
  aRecord <- abind::abind(aGrid,along=0) #along=0 binds on new dimension before first
  #! look at keeping names(dimnames(aRecordF))
  #! even with this they get lost later
  names(dimnames(aRecord)) <- c('day','x','y','sex','age')
  
  #for( day in 1:iDays ) {
  #changing to starting at day1, so first changes happen on day2
  #for( day in 2:iDays ) {
  #this ensures the loop isn't entered unless iDays is >1
  for( day in seq(from=2,length.out=iDays-1) ) {
    
    #####################
    ## adult mortality ##
    
    aGrid <- rtMortalityGrid( aGrid, 
                              vpMortF=vpMortF, 
                              vpMortM=vpMortM,
                              propDD=propMortAdultDD,
                              mCarryCap=mCarryCap )
    
    
    ##################
    ## adult ageing ##    
    aGrid <- rtAgeingGrid(aGrid)
    
    #the third dimension (age) loses it's label
    #just trying putting it back to see if that solves
    #"duplicated levels in factors are deprecated"
    #!this corrected the warnings
    #dimnames(aF) <- list(NULL,NULL,NULL)   
    names(dimnames(aGrid)) <- c('x','y','sex','age')
    
    
    #####################
    ## pupal emergence ##
    #this is a memory inefficient way of doing, creates copies of arrays
    #i do it this way to keep as much of the code in the function as possible
    l <- rtPupalEmergenceGrid( aGrid, aGridPup, iPupDurF=iPupDurF, iPupDurM=iPupDurM )
    aGrid <- l$aGrid 
    aGridPup <- l$aGridPup 
    
    ## pupal ageing ##
    aGridPup <- rtAgeingGrid(aGridPup, label="pupae")

  
    ###############
    ## fecundity ##    
    #set deposition rates by age
    vpDeposit <- rtSetDepositionRatesByAgeDI( iMaxAge=iMaxAge,
                                              iFirstLarva = iFirstLarva,
                                              iInterLarva = iInterLarva,
                                              pMortLarva = pMortLarva ) 
    
    #pupal ageing occurs immediately before this leaving a gap at age 1
    #this passes aGridPup to the deposition function to fill the age1 pupae there
    #also for now I'll pass aGrid and get the func to work out the numF in each grid cell   
    #uses the deposition rates set above
    aGridPup <- rtLarvalDepositionGrid( aGrid=aGrid, aGridPup=aGridPup, vpDeposit )    

    #the new age 1 pupae can be checked by (shows a grid each for M&F)
    #aGridPup[,,,'age1']
    
    #####################
    ## pupal mortality ##
    # is applied at day1 for the whole period
    # !note that iPupaDensThresh is currently constant across the grid
    aGridPup <- rtPupalMortalityGrid( aGridPup,
                                      pMort = pMortPupa, 
                                      propDD = propMortPupaDD,
                                      mCarryCap = mCarryCap )
    
    
    ####################
    ## movement adult ##
    #only if >1 row or col

    if( nRow > 1 | nCol > 1) {  
      
      #can nearly use apply to move both M&F in one command
      #aGrid2 <- apply(aGrid,MARGIN=c('age','sex'),function(m) rtMoveIsland(m, pMove=pMove))
      #but the x&y dimensions get combined and the dimnames get lost

      #Can move M&F in one line with aaply
      #checked and it does seem to work, but it fails with nRow,nCol=1
      #aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveIsland(m, pMove=pMove)) 
      #having margins .margins=c(1,2) didn't make movement work correctly
      
      #changing to reflecting boundaries
      aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveReflect(m, pMove=pMove)) 
      
      
      #put array dimensions back in correct order
      aGrid <- aperm(aGrid, c(3,4,1,2))
      
    }
        
    cat("day",day,"\n")
    
    #aF
    
    #dim(abind(x,y,along=0))     # binds on new dimension before first
    #dim(abind(x,y,rev.along=0)) # binds on new dimension after last
    aRecord <- abind::abind(aRecord, aGrid, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension
    
    
  } #end of iDays loop
  
  #ensuring that dimnames for the days dimension of aRecord is set
  #dimnames(aRecord)[[1]] <- paste0('day',0:iDays) #previously I started at day0
  dimnames(aRecord)[[1]] <- paste0('day',1:iDays) #previously I started at day0
  #resetting dimnames
  names(dimnames(aRecord)) <- c('day','x','y','sex','age')

  #produce a report on the model run, with text & graphs
  if (length(report)>0) rtReportPhase2( aRecord=aRecord, lNamedArgs=lNamedArgs, filename=report )


  #returning the popn record
  #! will need to modify later to return pupae too
  invisible(aRecord)
}

#accessing results
#tst <- rtPhase2Test2()

#tst['day0',,,'F','age1']
#tst['day1',,,'F','age1']
#aGrid <- tst['day1',,,,] #an array for one day
#tst['day1','x1','y1','F',] #an age structure for one cell on one day
#apply(tst,MARGIN=c('age'),sum) #summed age structure across grid over all days
#apply(tst,MARGIN=c('day','age'),sum) #summed age structure across grid for each day
#apply(tst,MARGIN=c('day','age','sex'),sum) #as above separated by MF
#apply(tst,MARGIN=c('x','y','day'),sum) #grid for each day all ages & sexes
#apply(tst['day14',,,,],MARGIN=c('x','y'),sum) #grid for a selected day all ages & sexes

# #testing plotting age structure by day summed across whole grid
# aS <- apply(tst,MARGIN=c('day','age'),sum) #summed age structure across grid for each day
# #> class(aS) [1] "matrix"
# #nearly works i think, except that dimensions for ages & days need to be swapped
# rtPlotAgeStructure(aS)
# aS2 <- aperm(aS,c(2,1))
# aS2[1,]
# rtPlotAgeStructure(aS2)
# #need to reverse ages, but difficulty is that if I use rev() it reverses the numbers but not the names !
# #reversing an individual dimension does work
# #rev(aS2[,2])
# aS3 <- apply(aS,MARGIN=c('day'),rev)

