library(rtsetse)

context("GetFromRecord array retrieval")

test_that("array retrievals work as expected", {
  
  #set up an example array
  nX <- 2
  nY <- 3
  iMaxAge <- 4
  nDays <- 2
  dimnames1 <- list( day=paste0('day',1:nDays), y=paste0('y',1:nY), x=paste0('x',1:nX), sex=c("F","M"), age=paste0('age',1:iMaxAge))
  nVals <- nDays*nY*nX*2*iMaxAge
  aRecord <- array(1:nVals, dim=c(nDays,nY,nX,2,iMaxAge), dimnames=dimnames1)
  
  #1st element of array
  expect_equal( rtGetFromRecord(aRecord, days=1, y=1, x=1, sex='F', age=1), aRecord[1,1,1,'F',1] )
  #last element of array
  expect_equal( rtGetFromRecord(aRecord, days=nDays, y=nY, x=nX, sex='M', age=iMaxAge), aRecord[nDays,nY,nX,'M',iMaxAge] )
  

})

