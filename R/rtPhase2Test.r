#rtPhase2Test.r
#starting to test simulating on a spatial grid with movement

#start with just F ageing & movement
#uses things developed in gridTest.r

#! check if it works when nRow & nCol == 1 (the aspatial model)

nRow <- 10
nCol <- 10
nAge <- 7
iDays <- 4

## creating array of [x,y,age]
dimnames <- list(NULL,NULL,NULL)
names(dimnames) <- c("x","y","age")
aF <- array(0, dim=c(nCol,nRow,nAge), dimnames=dimnames)
#adding 100 females of age 3 at 5,5
aF[5,5,3] <- 100
#also I can easily fill a constant age structure
#aF[5,5,] <- 100
#to test the aspatial model
#aF[1,1,3] <- 100

#to get a matrix for one age class
#aF[,,3]

for( day in 1:iDays ) {

  ##################
  ## adult ageing ##
  #.drop=FALSE makes it work for 1x1 grid
  aF <- aaply(aF, .margins=c(1,2), .drop=FALSE, rtAgeing )
  
  
  ##############
  ## movement ##
  aF <- aaply(aF, .margins=3, rtMove1 )
  #putting array components back in correct order
  aF <- aperm(aF, c(2, 3, 1))
  
  #cat("day",day,"\n")
  
  #aF
  
} #end of iDays loop



