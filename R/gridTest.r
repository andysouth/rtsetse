#gridTest.r
#andy south 3/6/14

#2nd temporary go at testing grid options for rtsetse
#a dimensioned list seems to be way forwards

library(rtsetse)

#this returns a list
lPopH <- rtPhase1Test()
lPopL <- rtPhase1Test(iStartAdults = 20)

#example of accessing the list
lPop$dfRecordPupaF$day30


#create a test grid with just the centre cell diff to others
nCol <- 3
nRow <- 3

#create an empty list
lGrid <- vector("list",nCol*nRow)
#adding the dimensions
dim(lGrid) <- c(nCol,nRow)

#can I fill all the cells with the low popn
#llply(lGrid,lPopL) #Error: attempt to apply non-function
# yes!
lGrid <- llply(lGrid,function(x) x<-lPopL)

#replacing the centreish cell 
#**AHA! solution seemed to be that I had to relist
lGrid[nCol/2+1,nRow/2+1] <- list(lPopH)

#now I should be able to access any cells
#remember [ gives as list [[ as contents
lGrid[[nCol/2+1,nRow/2+1]]


#inspecting the list with restricted recursion
str(lGrid, max.level=2)

# now if I wanted to create a map of popn in the grid
# I'd need to sum the popn in each cell
# operations on a list probably done with lapply
# or can I use dplyr ?? no that mostly works on dataframes
library(plyr)
library(dplyr)
library(raster)

#Cool first example to work, just getting max females in each cell
#and it returns a matrix (or a list with dimensions anyway)
tst <- llply(lGrid,function(x) max(x$dfRecordF)) #keeps dim but less useable list
#using lapply loses the dimensions
#tst2 <- lapply(lGrid,function(x) max(x$dfRecordF))
#I had to unlist it & stick the dimensions back on
#tst3 <- unlist(tst)
#dim(tst3) <- dim(tst)

#laply lost dim too but returns vector
tst <- laply(lGrid,function(x) max(x$dfRecordF)) 
dim(tst) <- dim(lGrid)
tstRast <- raster(tst)
plot(tstRast)

#how about getting total popn on day10
tst <- laply(lGrid,function(x) sum(x$dfRecordF$day10))
dim(tst) <- dim(lGrid)
tstRast <- raster(tst)
plot(tstRast)

#can start to see how I can write access functions that llply can use
#e.g. you specify day, gender & whether pupa or not

rtGetF <- function( lPop,
                    day = 0,
                    age = NA )
{
  
  sDayName <- paste0("day",day)
  
  #if no age passed, sum ages
  if ( is.na(age) )
  {
    fPop <- sum( lPop$dfRecordF[[sDayName]] )
  } else
  {
    #!beware that age currently reversed
    index <- nrow(lPop$dfRecordF) - age
    fPop <- lPop$dfRecordF[[sDayName]][index]
  }
  
  return(fPop)
}

#this means I can use pattern matching to do something like this
rtGetF(lPopL,d=1,a=1) #age1 on day1
rtGetF(lPopL,d=1) #all ages on day1

#and I can use that function to access values for the grid
tst <- laply(lGrid, rtGetF, day=1, age=1 )
dim(tst) <- dim(lGrid)
tstRast <- raster(tst)
plot(tstRast)

#now can I use a matrix of ‘habitat’ values to initiate populations ?
#although I am unlikely to want to run the gridded simulations using rtPhase1Test()
#i can try to use it to test this
#ideally I might use a matrix of input params to start simulations off in each grid cell
#however be careful that I'm not doing this in a way that's more difficult than it needs to be

#just as a thought, can I run rtPhaseTest for multiple cells using laply ?
#tst <- laply(lGrid, rtPhase1Test, iDays=5 )
#NO because this tries to use lGrid as the 2nd param for rtPhase1Test
#I want laply to ignore lGrid as a passing param
#but just run the function that many times & store the results in the list
#probably need a different version of *ply to do that

#I might just need to create a matrix of input params 
mStartAdults <- matrix(c(1:9),nrow=3)
###################################################################cool
#note plyr:llply to output results as a list
lGrid2 <- llply(mStartAdults, function(x) rtPhase1Test(iDays=5, iStartAdults=x) )
#!cool this works but again the dimensions are lost, can be put back
dim(lGrid2) <- dim(mStartAdults)

tst <- laply(lGrid2, rtGetF, day=0, age=0 )
dim(tst) <- dim(lGrid2)
tstRast <- raster(tst)
plot(tstRast)

#note I could easily set iCarryCap using the cool llply line above

###############################################
#now how to get flies to move from cell to cell

#worry about edges later
#just remind myself how Hat-trick does it, tricky see p16 in the manual
#simple version
#evacuation in 1 direction = mean daily displacement / cell width*4
#uses a lookup to choose evacuation rate that gives realistic 30 day displacement 
#daily displacement can also be modified by age, sex & vegetation
#(although little data for any of these)

#need to be slightly careful of double counting & overwriting
#will want a separate grid

#for each cell (& each age)
#simplest movement algo
#pop = pop - leavers + arrivers
#leavers = pop * probLeave
#arrivers = popNeighbour*probLeave/4
#will need some modifiability due to cell size & movement params

#thinking pseudocode to act on each cell
#a cell will be specified by [x,y]
#it's neighbours will be specified by 
x <- 2
y <- 2
neighbx <- x + c(0, 0, -1, 1)
neighby <- y + c(-1, 1, 0, 0)
#can I get the dimensions of this cell in a dimensioned list or matrix ?

m <- matrix(c(1:9),nrow=3)

#these give the row & column ids respectively
row(m)
col(m)
#but don't work when they are applied to a single element in an apply type func
#llply(m, function(x) paste(row(x),",",col(x),"\n") )

#getting there .....


