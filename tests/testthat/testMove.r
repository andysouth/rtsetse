library(rtsetse)

context("Movement")

test_that("vegetation dependent movement does what is expected", {
  
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  
  #these first three tests are just with movement from a single cell & include nogo
  
  #1 nogo neighbour and veg movement multiplier set to 1
  #expect rtMoveReflectNoGoVeg() to give same results as rtMoveReflectNoGo()
  mnog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  mveg <- mMovers
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mnog, mveg=mveg, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #as above but veg movement multiplier set to 2
  #expect 2* as many flies to move out
  mveg <- mMovers*2
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mnog, mveg=mveg, pMove=0.4),
                array(c(0, 0, 0, 0.2, 0.4, 0.2, 0, 0.2, 0, 0, 0, 0),dim=c(3,4)) )  
  #as above but veg movement multiplier set to 0.5
  #expect 0.5* as many flies to move out
  mveg <- mMovers/2
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mnog, mveg=mveg, pMove=0.4),
                array(c(0, 0, 0, 0.05, 0.85, 0.05, 0, 0.05, 0, 0, 0, 0),dim=c(3,4)) )  
  
  
  #now with 2 source cells and no nogo areas 
  mMovers <- array(c(0,0,0,0,1,0,0,1,0,0,0,0),dim=c(3,4))
  mnog <- array(c(1,1,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  
  #expect that when veg is 0 for a cell no movement from that cell
  mveg <- array(c(0,0,0,0,2,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mnog, mveg=mveg, pMove=0.4),
                array(c(0, 0.2, 0, 0.2, 0.2, 0.2, 0, 1.2, 0, 0, 0, 0),dim=c(3,4)) ) 
  
  #expect that when veg is 0.5 & 2 for neighbouring cells ...
  mveg <- array(c(0,0,0,0,0.5,0,0,2,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mnog, mveg=mveg, pMove=0.4),
                array(c(0, 0.05, 0, 0.05, 1, 0.05, 0.2, 0.25, 0.2, 0, 0.2, 0),dim=c(3,4)) )   

  
})

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
  #create an empty grid
  aGrid <- rtCreateGrid(nY=nY, nX=nX, nAge=iMaxAge, fill=0)  
  
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


