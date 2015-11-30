library(rtsetse)

context("Movement")

test_that("reduced movement down preferedness gradients works", {
  
  #first just test that it runs without a warning about number of movers changing
  mVegCats <- rtReadMapVeg( system.file("extdata","vegTanzaniaSerengetiTorr1km.txt", package="rtsetse"))
  #set 1 mover in all cells of a matrix
  mMovers <- array(1,dim=dim(mVegCats))
  #note this doesn't include vegetation dependent movement (mVegMove)
  rtMoveReflectNoGoVegBoundary(mMovers, mVegCats=mVegCats, iBestVeg = 4)
  rtMove(mMovers, mVegCats=mVegCats, iBestVeg = 4)
  
  #now how can I test a simple example where I know what I expect
  #in the e.g.s below iBestVeg set to 4 which is 'S' 
  #case : one central cell of preferred veg surrounded by veg of 1 less preferedness
  #expect: movement to neighbours to be 0.3*pMove/4
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)) 
  mVegCats <- array(c('B','B','B','B','S','B','B','B','B','B','B','B'),dim=c(3,4))  
  expect_equal( rtMoveReflectNoGoVegBoundary(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.03, 0, 0.03, 0.88, 0.03, 0, 0.03, 0, 0, 0, 0),dim=c(3,4))  )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.03, 0, 0.03, 0.88, 0.03, 0, 0.03, 0, 0, 0, 0),dim=c(3,4))  )

  #case : one central cell of preferred veg surrounded by veg of 2 less preferednesses
  #expect: movement to neighbours to be 0.1*pMove/4
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)) 
  mVegCats <- array(c('G','G','G','G','S','G','G','G','G','G','G','G'),dim=c(3,4))  
  expect_equal( rtMoveReflectNoGoVegBoundary(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.01, 0, 0.01, 0.96, 0.01, 0, 0.01, 0, 0, 0, 0),dim=c(3,4))  ) 
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.01, 0, 0.01, 0.96, 0.01, 0, 0.01, 0, 0, 0, 0),dim=c(3,4))  )  
  
  #case : one central cell of 1more dense veg('O') surrounded by veg of 1less dense('B')
  #expect: movement to neighbours to be 1*pMove/4
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4)) 
  mVegCats <- array(c('B','B','B','B','O','B','B','B','B','B','B','B'),dim=c(3,4))  
  expect_equal( rtMoveReflectNoGoVegBoundary(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))  )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mVegCats=mVegCats, iBestVeg=4, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))  ) 
  
  
})

test_that("vegetation dependent movement does what is expected", {
  
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  
  #these first three tests are just with movement from a single cell & include nogo
  
  #1 nogo neighbour and veg movement multiplier set to 1
  #expect rtMoveReflectNoGoVeg() to give same results as rtMoveReflectNoGo()
  mNog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  mVeg <- mMovers
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mNog, mVeg=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, mVegMove=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #as above but veg movement multiplier set to 2
  #expect 2* as many flies to move out
  mVeg <- mMovers*2
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mNog, mVeg=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.2, 0.4, 0.2, 0, 0.2, 0, 0, 0, 0),dim=c(3,4)) ) 
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, mVegMove=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.2, 0.4, 0.2, 0, 0.2, 0, 0, 0, 0),dim=c(3,4)) )   
  #as above but veg movement multiplier set to 0.5
  #expect 0.5* as many flies to move out
  mVeg <- mMovers/2
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mNog, mVeg=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.05, 0.85, 0.05, 0, 0.05, 0, 0, 0, 0),dim=c(3,4)) )  
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, mVegMove=mVeg, pMove=0.4),
                array(c(0, 0, 0, 0.05, 0.85, 0.05, 0, 0.05, 0, 0, 0, 0),dim=c(3,4)) )   
  
  #now with 2 source cells and no nogo areas 
  mMovers <- array(c(0,0,0,0,1,0,0,1,0,0,0,0),dim=c(3,4))
  mNog <- array(c(1,1,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  
  #expect that when veg is 0 for a cell no movement from that cell
  mVeg <- array(c(0,0,0,0,2,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mNog, mVeg=mVeg, pMove=0.4),
                array(c(0, 0.2, 0, 0.2, 0.2, 0.2, 0, 1.2, 0, 0, 0, 0),dim=c(3,4)) ) 
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, mVegMove=mVeg, pMove=0.4),
                array(c(0, 0.2, 0, 0.2, 0.2, 0.2, 0, 1.2, 0, 0, 0, 0),dim=c(3,4)) )
    
  #expect that when veg is 0.5 & 2 for neighbouring cells ...
  mVeg <- array(c(0,0,0,0,0.5,0,0,2,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflectNoGoVeg(mMovers, mNog, mVeg=mVeg, pMove=0.4),
                array(c(0, 0.05, 0, 0.05, 1, 0.05, 0.2, 0.25, 0.2, 0, 0.2, 0),dim=c(3,4)) )   
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, mVegMove=mVeg, pMove=0.4),
                array(c(0, 0.05, 0, 0.05, 1, 0.05, 0.2, 0.25, 0.2, 0, 0.2, 0),dim=c(3,4)) )    
})

test_that("movement avoiding no-go areas indeed avoids no-go areas", {
  
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))

  #0 nogo neighbours
  mNog <- array(c(1,1,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #1 nogo neighbour
  mNog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #2 nogo neighbours
  mNog <- array(c(1,0,1,0,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.8, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.8, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )  
  #3 nogo neighbours
  mNog <- array(c(1,0,1,0,1,0,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.9, 0, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 0.9, 0, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )  
  #4 nogo neighbours, all flies stay
  mNog <- array(c(1,0,1,0,1,0,1,0,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
                array(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  #reflection & nogo areas combined
  #mover in top left corner
  mMovers <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  mNog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  expect_equal( rtMoveReflectNoGo(mMovers, mNog, pMove=0.4),
                array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  #new streamlined rtMove()
  expect_equal( rtMove(mMovers, mNog, pMove=0.4),
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
  mNog <- array(c(1,0,1,1,1,1,1,1,1,1,1,1),dim=c(3,4))
  
  aGrid[,,'F',1] <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  #expected : array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4))
  aGrid[,,'M',2] <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  #expected : array(c(0, 0, 0, 0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))
  
  #movement avoiding no-go areas
  aGrid2 <- plyr::aaply(aGrid,.margins=c(3,4), .drop=FALSE,function(m) rtMoveReflectNoGo(m, mNog=mNog, pMove=0.4)) 
  
  #put array dimensions back in correct order
  aGrid2 <- aperm(aGrid2, c(3,4,1,2))
   
  #testing like this didn't work because of type difference (names) of the outputs
  #tests below are better
  #expect_equal( aGrid2[,,'F',1],
  #              array(c(0.9, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) ) 
  
  #test use of move function in aaply above vs against using on it's own 
  expect_equal( aGrid2[,,'F',1], rtMoveReflectNoGo(aGrid[,,'F',1], mNog=mNog, pMove=0.4) )  
  expect_equal( aGrid2[,,'M',2], rtMoveReflectNoGo(aGrid[,,'M',2], mNog=mNog, pMove=0.4) )
  #new streamlined rtMove()
  expect_equal( aGrid2[,,'F',1], rtMove(aGrid[,,'F',1], mNog=mNog, pMove=0.4) )  
  expect_equal( aGrid2[,,'M',2], rtMove(aGrid[,,'M',2], mNog=mNog, pMove=0.4) )  
  
  
  #B) check the overall size of the popn doesn't change
  expect_equal( sum(aGrid), sum(aGrid2) )
  
})  

test_that("reflective movement indeed reflects from boundaries", {
  
  #mover not on boundary
  mMovers <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4)) )  
  #mover in middle of left boundary
  mMovers <- array(c(0,1,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0.1, 0.7, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )   
  #mover in top left corner
  mMovers <- array(c(1,0,0,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0.8, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0.8, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )   
  #mover in top right corner
  mMovers <- array(c(0,0,0,0,0,0,0,0,0,1,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.8, 0.1, 0),dim=c(3,4)) )
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.8, 0.1, 0),dim=c(3,4)) )  
  #mover in lower right corner
  mMovers <- array(c(0,0,0,0,0,0,0,0,0,0,0,1),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.8),dim=c(3,4)) )
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.8),dim=c(3,4)) )
  #mover in lower left corner
  mMovers <- array(c(0,0,1,0,0,0,0,0,0,0,0,0),dim=c(3,4))
  expect_equal( rtMoveReflect(mMovers, pMove=0.4),
                array(c(0, 0.1, 0.8, 0, 0, 0.1, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )
  expect_equal( rtMove(mMovers, pMove=0.4),
                array(c(0, 0.1, 0.8, 0, 0, 0.1, 0, 0, 0, 0, 0, 0),dim=c(3,4)) )  
})

test_that("movement options don't generate warnings assoc with solved problem from 5/6/15", {
  
  mVegCats = array(c("D","T","O","S","N","N"),dim=c(2,3))
  m = array(c(1,1,1,1,1,1),dim=c(2,3))
  mNog = array(c(1,1,1,1,0,0),dim=c(2,3))
  dfMoveByVeg =  data.frame(code=c("D","T","O","S","B","G","N"),move=c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 0))
  iBestVeg = 4
  pMove = 0.4
  
  #create an array to be used in modifying movement from a cell based on it's vegetation
  aVegMoveMult <- rtSetVegMoveGrids( mVegCats = mVegCats, dfMoveByVeg = dfMoveByVeg )
  
  #create a 2nd array used in reducing movement into less preferred cells
  aVegDifMult <- rtSetVegDifGrids( mVegCats = mVegCats, iBestVeg = iBestVeg )
  
  #nogo effect alone
  expect_warning( tst <- rtMove(m, verbose=TRUE, mNog=mNog),
                  NA )
  
  #vegetation movement multiplier & nogo effect
  expect_warning(  tst <- rtMove(m, aVegMoveMult=aVegMoveMult,verbose=TRUE, mNog=mNog),
                   NA )
  
  #vegetation movement multiplier & veg difference effect
  expect_warning( tst <- rtMove(m, aVegMoveMult=aVegMoveMult, aVegDifMult=aVegDifMult,verbose=TRUE),
                  NA )
  
  #veg difference effect alone
  expect_warning( tst1 <- rtMove(m, aVegDifMult=aVegDifMult,verbose=TRUE),
                  NA )
  
  #veg difference effect alone specified by mVegCats & iBestVeg
  expect_warning( tst2 <- rtMove(m, mVegCats=mVegCats, iBestVeg=iBestVeg, verbose=TRUE),
                  NA )
  
  #previous two should produce same result
  expect_equal(sum(tst1-tst2),0)
  
})

test_that("moving M&F differently works", {
  
  #create a test grid array, y,x,sex,age
  nX <- 4
  nY <- 3
  iMaxAge <- 2
  #create an empty grid
  aGrid <- rtCreateGrid(nY=nY, nX=nX, nAge=iMaxAge, fill=0)  

  pMoveF <- 0.6
  pMoveM <- 0.3  
  
  aGrid[,,'F',2] <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  aGrid[,,'M',2] <- array(c(0,0,0,0,1,0,0,0,0,0,0,0),dim=c(3,4))
  #expected F0.6 : array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))
  #expected M0.3 : array(c(0, 0.1, 0, 0.1, 0.6, 0.1, 0, 0.1, 0, 0, 0, 0),dim=c(3,4))  
  
  aAgeyx <- plyr::aaply(aGrid[,,'F',], .margins=c(3),.drop=FALSE,function(m) rtMoveReflect(m, pMove=pMoveF))
  #put dimensions back in correct order
  aGrid[,,'F',] <- aperm(aAgeyx, c(2,3,1))
 
  aAgeyx <- plyr::aaply(aGrid[,,'M',], .margins=c(3),.drop=FALSE,function(m) rtMoveReflect(m, pMove=pMoveM))
  #put dimensions back in correct order
  aGrid[,,'M',] <- aperm(aAgeyx, c(2,3,1)) 
  
  #expect_equivalent tests with all.equal and check.attributes = FALSE
  #others fails because attriubutes get lost in movement
  
  expect_equivalent( aGrid[,,'F',2],
                     array(c(0, 0.15, 0, 0.15, 0.4, 0.15, 0, 0.15, 0, 0, 0, 0),dim=c(3,4))  ) 
  
  expect_equivalent( aGrid[,,'M',2],
                     array(c(0, 0.075, 0, 0.075, 0.7, 0.075, 0, 0.075, 0, 0, 0, 0),dim=c(3,4))  ) 
    
})
  