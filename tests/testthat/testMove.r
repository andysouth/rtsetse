library(rtsetse)

context("Movement")

test_that("movement avoiding no-go areas indeed avoids no-go areas", {
  
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))

  #0 nogo neighbours
  mnog <- array(c(1,1,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #1 nogo neighbour
  mnog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #2 nogo neighbours
  mnog <- array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.8, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #3 nogo neighbours
  mnog <- array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.9, 0, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #4 nogo neighbours, all flies stay
  mnog <- array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )
  
  #reflection & nogo areas combined
  #mover in top left corner
  mMovers <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  mnog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mnog, pMove=0.4),
                array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  
})

test_that("movement of multiple age classes using plyr::aaply works", {
  
  #create a test grid array, y,x,sex,age
  nX <- 4
  nY <- 3
  iMaxAge <- 2
  dimnames1 <- list( y=paste0('y',1:nY), x=paste0('x',1:nX), sex=c("F","M"), age=paste0('age',1:iMaxAge))
  #create an empty grid
  aGrid <- array(0, dim=c(nY,nX,2,iMaxAge), dimnames=dimnames1)
  
  #try repeating some of the tests for movement avoiding no-go areas from above
  #for different age classes
  #A) check they give correct results within each agesex class
  #B) check the overall size of the popn doesn't change
  mnog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  
  aGrid[,,'F',1] <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  #expected : array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4))
  aGrid[,,'M',2] <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  #expected : array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))
  
  #movement avoiding no-go areas
  aGrid2 <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveReflectNoGo(m, mnog=mnog, pMove=0.4)) 
  
  #put array dimensions back in correct order
  aGrid2 <- aperm(aGrid2, c(3,4,1,2))
   
  #testing like this didn't work because of type difference (names) of the outputs
  #tests below are better
  #expect_equal( aGrid2[,,'F',1],
  #              array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) ) 
  
  #test use of move function in aaply above vs against using on it's own 
  expect_equal( aGrid2[,,'F',1],
                rtMoveReflectNoGo(aGrid[,,'F',1], mnog=mnog, pMove=0.4) )  
  expect_equal( aGrid2[,,'M',2],
                rtMoveReflectNoGo(aGrid[,,'M',2], mnog=mnog, pMove=0.4) )   
  
  #B) check the overall size of the popn doesn't change
  expect_equal( sum(aGrid), sum(aGrid2) )
  
})  

test_that("reflective movement indeed reflects from boundaries", {
  
  #mover not on boundary
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  
  #mover in middle of left boundary
  mMovers <- array(c(0,1,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  
  #mover in top left corner
  mMovers <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0.8, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  
  #mover in top right corner
  mMovers <- array(c(0,0,0,0,0,0,0,0,0,1,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.8, 0.1, 0),dim=c(3,4)) )
  
  #mover in lower right corner
  mMovers <- array(c(0,0,0,0,0,0,0,0,0,0,0,1),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.8),dim=c(3,4)) )

  #mover in lower left corner
  mMovers <- array(c(0,0,1,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0.1, 0.8, 0, 0, 0.1, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )
})

