

#I could put the mVegMove stuff in here too
#but then how would I name the storage in the returned array
#does it become less clear ?

#I could have 2 functions
#rtSetVegMoveGrids : returns aVegMoveMult[NSEW]
#rtSetVegDifMoveGrids : returns aVegDifMult[NSEW,NS,EW etc.]


rtSetVegDifMoveGrids <- function(mVegCats = array(c("O","O","O","O","S","O","O","O","O","O","O","O"),dim=c(3,4)),
                           iBestVeg = 4,
                           verbose=FALSE) {

  ################################################################
  # to next line of # can be put outside the function
  # then perhaps I pass the following array 
  #aVegDifMult[y,x,(N,E,S,W,SN,WE,NS,EW)]
  
  
  nY <- dim(mVegCats)[1]
  nX <- dim(mVegCats)[2]
  dimnames1 <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), grid=c("N","E","S","W","SN","WE","NS","EW"))

  #dim of array got from dimnames above
  aVegDifMult <- array(dim=sapply(dimnames1,length), dimnames=dimnames1)
   
  
  #convert veg characters to numeric, note nogo "N" to NA
  mVegNum <- rtSetGridFromVeg( mVegCats, dfLookup=data.frame(from=c("D","T","O","S","B","G","N"),to=c(1,2,3,4,5,6,NA),stringsAsFactors = FALSE ))
  
  #matrix of difference of veg in cell from best
  mVegDifPref <- abs(iBestVeg-mVegNum)
  
  #BEWARE what to do with nogo areas
  #try to keep it simple, it shouldn't matter because no flies in them.
  #can I just convert them to NA ?
  #NO this wouldn't work ... convert any difference >5 (caused by nogo areas of 99)to 6
  
  #remember
  #in the code below matrices with NESW on end are source cells
  #matrices without are destination cells
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #so I need to create NESW matrices that represent the change in veg preference associated with that move
  #first create copies
  mVegDifPrefN <- shiftGridReflectN(mVegDifPref)
  mVegDifPrefE <- shiftGridReflectE(mVegDifPref)
  mVegDifPrefS <- shiftGridReflectS(mVegDifPref)   
  mVegDifPrefW <- shiftGridReflectW(mVegDifPref)      
  #then do calculation, source-destination
  mVegDifPrefN <- mVegDifPrefN - mVegDifPref
  mVegDifPrefE <- mVegDifPrefE - mVegDifPref
  mVegDifPrefS <- mVegDifPrefS - mVegDifPref
  mVegDifPrefW <- mVegDifPrefW - mVegDifPref
  #seems to work
  #unique(as.vector(mVegDifPrefN))
  #now convert the change associated with the move to a modifier of the movement rate
  #convert 0:5 to   1, 0.3, 0.1, 0.03, 0.01, 0.001 
  #dfLookup <- data.frame(from=c(-5:5),to=c(0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1))
  #include converting any NAs to 0 so no flies move to or from
  dfLookup <- data.frame(from=c(NA,-5:5),to=c(0, 0.001, 0.01, 0.03, 0.1, 0.3, 1, 1, 1, 1, 1, 1))
  #mVegbmult mVeg boundary multiplier
  #these are used for calculating arrivers
  #for each cell they are the difference in preference with 4 neighbours that act as sources
  aVegDifMult[,,"N"] <- rtSetGridFromVeg( mVegDifPrefN, dfLookup=dfLookup )
  aVegDifMult[,,"E"] <- rtSetGridFromVeg( mVegDifPrefE, dfLookup=dfLookup )
  aVegDifMult[,,"S"] <- rtSetGridFromVeg( mVegDifPrefS, dfLookup=dfLookup )
  aVegDifMult[,,"W"] <- rtSetGridFromVeg( mVegDifPrefW, dfLookup=dfLookup )
  
  #BEWARE 5/3/15 THIS IS THE TRICKIEST BIT IN THE WHOLE OF RTSETSE
  #I've tested that it does do what is expected, e.g. see movement vignette
  #but how the mechanism is coded to be time efficient is very tricky
  
  #the below are needed for calculating stayers
  #for each cell they are the difference in preference with 4 neighbours that act as sinks
  #they are calculated by going back the other way from the previous calculation
  #all boundary values are replaced with 1 because for reflecting boundaries there will be no change 
  #in vegetation associated with movements in and out of the grid    
  aVegDifMult[,,"SN"] <- shiftGridIslandN( mVegbmultS, fill=1 )
  aVegDifMult[,,"WE"] <- shiftGridIslandE( mVegbmultW, fill=1 )
  aVegDifMult[,,"NS"] <- shiftGridIslandS( mVegbmultN, fill=1 )   
  aVegDifMult[,,"EW"] <- shiftGridIslandW( mVegbmultE, fill=1 )   

  invisible(aVegDifMult)
}