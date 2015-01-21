library(rtsetse)

context("Control")

test_that("control works when age-independent & constant except for border", {

  #create a test grid array, y,x,sex,age
  nX <- 4
  nY <- 3
  iMaxAge <- 2
  #create a grid filled with 1s
  aGrid <- rtCreateGrid(nY=nY, nX=nX, nAge=iMaxAge, fill=1)  
  #apply control
  pControl <- 0.1
  aGrid2 <- rtControlGrid(aGrid, pControl=pControl, iControlBorder=1)
  
  #expect no control in a border cell
  expect_equal( aGrid2[1,1,'F',1],
                aGrid[1,1,'F',1] )  
  #expect control in a non border cell
  expect_equal( aGrid2[2,2,'F',1],
                aGrid[2,2,'F',1] * (1-pControl) )  
   
  })

test_that("control works when age-independent & varies across a  passed grid", {
  
  #create a test grid array, y,x,sex,age
  nX <- 4
  nY <- 3
  iMaxAge <- 2
  #create a grid filled with 1s
  aGrid <- rtCreateGrid(nY=nY, nX=nX, nAge=iMaxAge, fill=1)  
  #create a control modifying matrix
  #start with all 1s
  mGridControlModifier <- matrix(1,nrow=nY,ncol=nX)
  mGridControlModifier[2,2] <- 0.5
  mGridControlModifier[2,3] <- 2
  
  #apply control
  pControl <- 0.1
  aGrid2 <- rtControlGrid(aGrid, pControl=pControl, iControlBorder=1, mGridControlModifier=mGridControlModifier)
  
  #expect no control in a border cell
  expect_equal( aGrid2[1,1,'F',1],
                aGrid[1,1,'F',1] )  
  #expect half control in this non border cell
  expect_equal( aGrid2[2,2,'F',1],
                aGrid[2,2,'F',1] - ( aGrid[2,2,'F',1] * mGridControlModifier[2,2] * pControl )  ) 
  #expect twice control in this non border cell
  expect_equal( aGrid2[2,3,'F',1],
                aGrid[2,3,'F',1] - ( aGrid[2,3,'F',1] * mGridControlModifier[2,3] * pControl )  )   
  
})