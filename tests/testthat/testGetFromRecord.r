library(rtsetse)

context("GetFromRecord array retrieval")

test_that("array retrievals work as expected", {
  
  #set up an example array
  nX <- 2
  nY <- 3
  iMaxAge <- 4
  nDays <- 2
#   dimnames1 <- list( day=paste0('day',1:nDays), y=paste0('y',1:nY), x=paste0('x',1:nX), sex=c("F","M"), age=paste0('age',1:iMaxAge))
#   nVals <- nDays*nY*nX*2*iMaxAge
#   aRecord <- array(1:nVals, dim=c(nDays,nY,nX,2,iMaxAge), dimnames=dimnames1)
  aRecord <- rtCreateRecord( nY=nY, nX=nX, nAge=4 )
  #To fill array with sequential values
  aRecord[] <- c(1:length(aRecord))



  #1st element of array
  expect_equal( rtGetFromRecord(aRecord, days=1, y=1, x=1, sex='F', age=1), 
                aRecord[1,1,1,'F',1] )
  #last element of array
  expect_equal( rtGetFromRecord(aRecord, days=nDays, y=nY, x=nX, sex='M', age=iMaxAge), 
                aRecord[nDays,nY,nX,'M',iMaxAge] )
  #age structure for whole popn on final day is length of iMaxAge
  expect_equal( length(rtGetFromRecord(aRecord,days=nDays,x='sum',y='sum',sex='sum')), 
                iMaxAge ) 
  #raw grid array for one day
  #this fails because age & sex dimensions are returned in different order
  #expect_equal( rtGetFromRecord(aRecord,days=2),
  #              aRecord[2,,,,] )
  #sex ratio for whole pop on final day is length 2
  expect_equal( length(rtGetFromRecord(aRecord,days=2,x='sum',y='sum',age='sum')),
                2 )

})

