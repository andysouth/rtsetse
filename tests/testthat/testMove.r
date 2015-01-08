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


