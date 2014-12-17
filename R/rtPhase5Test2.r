#' spatial tsetse population simulation with mortality varying over the grid
#'
#' \code{rtPhase5Test2} replaces \code{rtPhase5Test}
#' 
#' This accepts a grid of vegetation and how mortality varies by vegetation

#' @param mVegetation a matrix or filepath for a map of vegetation codes
#' @param dfMortByVeg a dataframe or filepath to a lookup table specifying mortality multiplier (percent) for each vegetation type
# @param mCarryCapF a matrix of female carrying capacities
# @param nCol number grid columns
# @param nRow number grid rows
#' @param pMove probability of moving between cells
# following are same as rtPhase1Test3
#' @param iDays days to run simulation
#' @param iMaxAge max age of fly allowed in model (will warn if flies age past this)
#' @param iCarryCapF carrying capacity of adult females 
#' @param fMperF numbers of males per female, default 0.5 for half as many M as F
#' @param fStartPopPropCC starting population as a proportion of carrying capacity, default = 1
# @param iStartAdults number of adults to start simulation with
# @param iStartAges spread start adults across the first n ages classes
# @param iStartPupae number of pupae to start simulation with (they get spread across sex&age)
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
#' @param pControl extra mortality probability due to control, first go at implementing control. TODO improve this
#' @param verbose print what it's doing T/F
#' @param report filename for a report for this run, if not specified no report is produced

#' @return a multi-dimensional array [day,x,y,sex,ages]
#' @examples
#' \dontrun{
#' tst <- rtPhase5Test2()
#' rtPlotMapPop(tst)
#' #now try an unequal matrix
#' mCarryCapF <- matrix( 10*(1:16), 4, 4)
#' tst <- rtPhase5Test2(mCarryCapF)
#' rtPlotMapPop(tst) 
#' #an unequal matrix starting at a fraction of CC
#' tst <- rtPhase5Test2(mCarryCapF, fStartPopPropCC = 0.5)
#' rtPlotMapPop(tst) 
#' }
#' @export
#' 
rtPhase5Test2 <- function( mVegetation = array(c("D","T","O","S","N","N"),dim=c(2,3)),
                          dfMortByVeg = data.frame(code=c("D","T","O","S","B","G","N"),mortality=c(200,150,110,100,110,210,999),pupmortality=c(120,110,105,100,120,170,999),stringsAsFactors = FALSE),
#                           mCarryCapF = matrix(200,4,4),
#                           nCol = 10,
#                           nRow = 10,
                          pMove = 0.4,
                          iDays = 4,
                          iMaxAge = 120,
                          iCarryCapF = 200,
                          fMperF = 0.5,           
                          fStartPopPropCC = 1,
                          #iStartAdults = 200,
                          #iStartAges = 1,
                          #iStartPupae = "sameAsAdults",
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
                          pControl = 0,
                          verbose = FALSE,
                          report = NULL ) #"reportPhase2.html" ) 
{
  
  ##some argument checking
  #if( nRow < 2 | nCol < 2 )
  #  stop("movement does not work if less than 2 grid rows or columns")

  #getting the arguments
  lNamedArgs <- mget(names(formals()),sys.frame(sys.nframe()))
    
  if (verbose) cat("starting rtPhase5Test2 with arguments:",paste0(names(lNamedArgs),"=",lNamedArgs,","),"\n")
  
  #read in the vegetation map if it has been specified as a filepath
  if ( class(mVegetation) =="character" )
  {
    mVegetation <- rtReadMapVeg(mVegetation)
  }
  #read in the attribute file if it has been specified as a filepath  
  if ( class(dfMortByVeg) =="character" )
  {
    #TODO add some checks to this
    if (!file.exists(dfMortByVeg)) stop("your specified vegetation attribute file seems not to exist here:",dfMortByVeg)
    dfMortByVeg <- read.csv(dfMortByVeg, as.is=TRUE) 
  }
  
  
  #age dependent mortality
  #beware the first arg is mortality on day1 rather than average mortality
  #todo: change to pass iMaxAge rather than vPop
  vpMortF <- rtSetMortRatesByAge( vPop=c(1:iMaxAge), 
                                  pMortAge1 = pMortF,
                                  iMortMinAgeStart = iMortMinAgeStart,
                                  iMortMinAgeStop = iMortMinAgeStop,
                                  fMortMinProp = fMortMinProp,
                                  fMortOldProp = fMortOldProp )  
  #todo: change to pass iMaxAge rather than vPop
  vpMortM <- rtSetMortRatesByAge( vPop=c(1:iMaxAge), 
                                  pMortAge1 = pMortM,
                                  iMortMinAgeStart = iMortMinAgeStart,
                                  iMortMinAgeStop = iMortMinAgeStop,
                                  fMortMinProp = fMortMinProp,
                                  fMortOldProp = fMortOldProp ) 
  
  #create mortality multiplier grid from the vegetation grid
  mMortMultGrid <- rtSetMortGridFromVeg( mVegetation = mVegetation,
                                         dfMortByVeg = dfMortByVeg )  
  #if (verbose) cat("mortality multiplier grid set to:",mMortMultGrid,"\n")

  #setting a total carryCap from the female input
  iCarryCap <- iCarryCapF * (1+fMperF)

  #get nCol&nRow form the vegetation matrix
  #nCol <- ncol(mVegetation)
  #nRow <- nrow(mVegetation)
  # y,x matrix indexing
  nY <- dim(mVegetation)[1]
  nX <- dim(mVegetation)[2]

  #mnog a matrix of cells of 0&1, 0 for nogo areas, used in movement 
  #name dimensions to try to avoid confusion with x,y
  mnog <- ifelse( mVegetation=="N",0,1)
  #BEWARE! confusion bewteen x&y dimensions
  #mnog <- t(mnog) #transpose
  #TODO not sure whether to name Y nY:1 or 1:nY
  dimnamesMatrix <- list( y=paste0('y',1:nY), x=paste0('x',1:nX) )
  dimnames(mnog) <- dimnamesMatrix
  
  
  #create arrays of 0s for pupae & adults to start
  #PUPAE ----
  iMaxPupAge <- max(iPupDurM, iPupDurF)
  dimnamesPup <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), sex=c("F","M"), age=paste0('age',1:iMaxPupAge))
  aGridPup <- array(0, dim=c(nY,nX,2,iMaxPupAge), dimnames=dimnamesPup)  
  #ADULTS
  dimnames1 <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), sex=c("F","M"), age=paste0('age',1:iMaxAge))
  aGrid <- array(0, dim=c(nY,nX,2,iMaxAge), dimnames=dimnames1)    

  #loop for each cell in the grid
  #to enable initiation of pupae and adults at carrycap in each
  #more transparent to go through x&y than all elements of the matrix
  if (verbose) cat("starting loop to fill grid of y,x=",nY,", ",nX,"\n")  
  
  for( y in 1:nY ) #y
  {
    for( x in 1:nX ) #x
    {             
      #don't put flies into no-go areas
      if ( mnog[y,x] != 0 )        
      {      
        #TODO check whether pupae have to be calculated for each cell
        #vpMortF is passed, do I need to apply the veg mortality multiplier ?
        #even if I do, I could do it once for each veg type 
        #and store before the row,col loop  
        #likewise for adults
          
        #calculating start numbers of pupae
        fPupaPerSexAge <- rtCalcPupaPerSexAge(pMortPupa = pMortPupa, 
                                              vpMortF = vpMortF,
                                              fStartPopPropCC = fStartPopPropCC,
                                              iCarryCapF = iCarryCapF)
        
        #vectors for pupae filled with same number of pupae at all ages
        #because males stay in the ground longer this means there will be more males 
        vPupaM <- rep(fPupaPerSexAge, iPupDurM)
        #make the F vector up to the same length as the M with extra 0's
        vPupaF <- c(rep(fPupaPerSexAge, iPupDurF),rep(0,iPupDurM-iPupDurF))
        #then put each pupal vector into the array
        aGridPup[y, x, 'F', ] <- vPupaF
        aGridPup[y, x, 'M', ] <- vPupaM      
        
        #adults per cell based on fPupaPerSexAge for this cell
        #start age structure at stability
        vPopStartF <- rtSetAgeStructure(vpMortF, fPopAge0=fPupaPerSexAge)
        vPopStartM <- rtSetAgeStructure(vpMortM, fPopAge0=fPupaPerSexAge)
        
        #adding adults to this cell
        aGrid[y, x,'F', ] <- vPopStartF
        aGrid[y, x,'M', ] <- vPopStartM  
      } #end if not no-go area
    } #end y    
  } #end x

#22/9/14 end of part changed for rtPhase5Test()

  
  
# to access array dimensions by name 
#   aGrid['y1','x1','M',] #an age structure for one cell
#   sum(aGrid['y1','x1','M',]) #total M in one cell
#   sum(aGrid['y1','x1',,]) #total pop in one cell
#   aGrid[,,'M','age2'] #a grid of one age  
#   aGrid[,,'F',] #grid of age structures just for F
#   apply(aGrid,MARGIN=c('y','x'),sum) #grid for all ages & sexes
#   apply(aGrid,MARGIN=c('age'),sum) #summed age structure for whole pop
#   apply(aGrid,MARGIN=c('sex'),sum) #summed sex ratio for whole pop  
  
  
  # the most sensible way to save popn record
  # would seem to be to use abind to just add another dimension
  #library(abind)
  aRecord <- abind::abind(aGrid,along=0) #along=0 binds on new dimension before first
  #! look at keeping names(dimnames(aRecordF))
  #! even with this they get lost later
  names(dimnames(aRecord)) <- c('day','y','x','sex','age')
  

  if (verbose) cat("starting loop for",iDays,"days\n")

  #for( day in 1:iDays ) {
  #changing to starting at day1, so first changes happen on day2
  #for( day in 2:iDays ) {
  #this ensures the loop isn't entered unless iDays is >1
  for( day in seq(from=2,length.out=iDays-1) ) {
    
    cat("day",day," of ",iDays,"\n")
    
    #####################
    ## adult mortality ##
    if (verbose) cat("imposing adult mortality popbefore=",sum(aGrid))
    
    aGrid <- rtMortalityGrid( aGrid, 
                              vpMortF=vpMortF, 
                              vpMortM=vpMortM,
                              mMortMultGrid=mMortMultGrid,
                              propDD=propMortAdultDD,
                              iCarryCap=iCarryCap )
    
    if (verbose) cat(" popafter=",sum(aGrid),"\n")    
    
    ##################
    ## adult ageing ##    
    #if (verbose) cat("ageing\n")
    aGrid <- rtAgeingGrid(aGrid)
    
    #the third dimension (age) loses it's label
    #"duplicated levels in factors are deprecated"
    #this corrected the warnings
    names(dimnames(aGrid)) <- c('y','x','sex','age')
    
    
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

    if (verbose) cat("pupae deposited =", sum(aGridPup[,,,'age1']), "\n" )
    
    #the new age 1 pupae can be checked by (shows a grid each for M&F)
    #aGridPup[,,,'age1']
    
    #####################
    ## pupal mortality ##
    # is applied at day1 for the whole period
    # !note that iPupaDensThresh is currently constant across the grid
    aGridPup <- rtPupalMortalityGrid( aGridPup,
                                      pMort = pMortPupa, 
                                      propDD = propMortPupaDD,
                                      iCarryCap = iCarryCap )
    
    
    ####################
    ## movement adult ##
    
    #errors if x or y is 1
    if( nX > 1 | nY > 1) {  
      
      #can nearly use apply to move both M&F in one command
      #aGrid2 <- apply(aGrid,MARGIN=c('age','sex'),function(m) rtMoveIsland(m, pMove=pMove))
      #but the x&y dimensions get combined and the dimnames get lost

      #Can move M&F in one line with aaply
      #checked and it does seem to work, but it fails with nRow,nCol=1
      #aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveIsland(m, pMove=pMove)) 
      #having margins .margins=c(1,2) didn't make movement work correctly
      
      #changing to reflecting boundaries
      #aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveReflect(m, pMove=pMove)) 
      
      #movement avoiding no-go areas
      aGrid <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveReflectNoGo(m, mnog=mnog, pMove=pMove)) 
      
      #put array dimensions back in correct order
      aGrid <- aperm(aGrid, c(3,4,1,2))
      
    }
    
    #4/12/14 first go at implementing control
    #TODO make this better, just temporarily here !!
    if (pControl > 0 ) aGrid <- rtControlTestGrid1(aGrid, pControl=pControl)
    
    
    #dim(abind(x,y,along=0))     # binds on new dimension before first
    #dim(abind(x,y,rev.along=0)) # binds on new dimension after last
    aRecord <- abind::abind(aRecord, aGrid, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension
    #seems these dimension names get lost
    dimnames(aRecord)[[1]] <- paste0('day',1:day) #just goes to the current day 
    names(dimnames(aRecord)) <- c('day','y','x','sex','age')    
    
    
    if (verbose) cat("adult popn =",rtGetFromRecord(aRecord,days=day,y='sum',x='sum',sex='sum',age='sum'),"\n")
    #sum(aRecord[day,,,,]) #gives same
  
    
  } #end of iDays loop
  
  #ensuring that dimnames for the days dimension of aRecord is set
  dimnames(aRecord)[[1]] <- paste0('day',1:iDays) #previously I started at day0, with 0:iDays
  #resetting dimnames
  names(dimnames(aRecord)) <- c('day','y','x','sex','age')

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
#tst['day1','y1','x1','F',] #an age structure for one cell on one day
#apply(tst,MARGIN=c('age'),sum) #summed age structure across grid over all days
#apply(tst,MARGIN=c('day','age'),sum) #summed age structure across grid for each day
#apply(tst,MARGIN=c('day','age','sex'),sum) #as above separated by MF
#apply(tst,MARGIN=c('y','x','day'),sum) #grid for each day all ages & sexes
#apply(tst['day14',,,,],MARGIN=c('y','x'),sum) #grid for a selected day all ages & sexes

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

