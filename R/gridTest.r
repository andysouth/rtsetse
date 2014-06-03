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


