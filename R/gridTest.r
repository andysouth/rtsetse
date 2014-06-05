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
row(m)+1 #gives the row of the cells above each cell

#clockwise neighbours starting at 12
vNeighb4x <- c(0, 1, 0, -1)
vNeighb4y <- c(1, 0, -1, 0)

#row(m) + vNeighb4x
#longer object length is not a multiple of shorter object length

paste0(row(m),",",col(m))
#gives coords of all cells but loses dimensions, may not matter
#[1] "1,1" "2,1" "3,1" "1,2" "2,2" "3,2" "1,3" "2,3" "3,3"

#Vectorize may help
#llply(m, Vectorize(function(x) paste(row(x),",",col(x),"\n") ))
#failed
#llply(row(m), col(m), Vectorize(function(x,y) paste(x,",",xy,"\n") ))
#failed

f <- function(row_index, col_index) {paste0(row_index,",",col_index)}
f(row(m), col(m))
#works BUT I just want to be able to pass m to the function ?
#or should I learn how to pass multiple args to functions in *apply

#ah! the SO questioner had probalem that their function was using value rather than index
#actually I want to use value
#mply functions don't seem to do what I want, they take multiple arguments from columns of a dataframe

#trying different way
#http://www.r-bloggers.com/fast-conways-game-of-life-in-r/
#making offset copies of a matrix
#allW = cbind( rep(0,side) , X[,-side] )
#allNW = rbind(rep(0,side),cbind(rep(0,side-1)
#allN = rbind(rep(0,side),X[-side,])
#allNE = rbind(rep(0,side),cbind(X[-side,-1],rep(0,side-1)))
#allE = cbind(X[,-1],rep(0,side))
#allSE = rbind(cbind(X[-1,-1],rep(0,side-1)),rep(0,side))
#allS = rbind(X[-1,],rep(0,side))
#allSW = rbind(cbind(rep(0,side-1),X[-1,-side]),rep(0,side))

#creating a bigger matrix to test the below
m <- matrix(0, nrow=10, ncol=10)
m[5,5] <- 100

##########LOOKING GOOD
#this puts zeros in, but I could modify it to copy the boundary cell
#simply by replacing these bits rep(0,nrow(m))
mW = cbind( rep(0,nrow(m)), m[,-nrow(m)] )
mN = rbind( rep(0,ncol(m)), m[-ncol(m),] )
mE = cbind( m[,-1], rep(0,nrow(m)) )
mS = rbind( m[-1,], rep(0,ncol(m)) )

#check seems right
#mN
#mE
#mS
#mW

#prelim movement test
pMove <- 0.25
mArrivers <- pMove*(mN + mE + mS + mW)/4
mStayers <- (1-pMove)*m
mNew <- mArrivers + mStayers
mNew
m <- mNew #ready to repeat

#thinking about movement & age structure
#currently I have a vector of ages in each matrix cell
#I might need to extract a matrix for each age
#do the movement & then put it back into each vector
#that sounds time costly
#ideally I would pass the matrix of vectors and it would sort it ...
#seems unlikely

rtMove1 <- function(m, pMove=0.4)
{
  #this puts zeros in, but I could modify it to copy the boundary cell
  #simply by replacing these bits rep(0,nrow(m))
  mW = cbind( rep(0,nrow(m)), m[,-nrow(m)] )
  mN = rbind( rep(0,ncol(m)), m[-ncol(m),] )
  mE = cbind( m[,-1], rep(0,nrow(m)) )
  mS = rbind( m[-1,], rep(0,ncol(m)) )
  
  #check seems right
  #mN
  #mE
  #mS
  #mW
  
  mArrivers <- pMove*(mN + mE + mS + mW)/4
  mStayers <- (1-pMove)*m
  
  mNew <- mArrivers + mStayers
  return( mNew )
  #m <- mNew #ready to repeat
}

#getting a single age into a matrix
mGridFAge1 <- laply(lGrid, rtGetF, day=1, age=1 )
dim(mGridFAge1) <- dim(lGrid)
#testing movement on that object
rtMove1(mGridFAge1)
#works OK
#how would I put that back into each age class in the vectors
#might need an rtSetF

#day is only here temporarily
#likely that I'll only want to do this for current day
#day stuff will be dealt with in recording
#lPop the list of popn stuff held for each cell
#fPop the value to use to set pop for that age
rtSetF <- function( lPop,
                    fPop,
                    day = 0,
                    age = 0 )
{
  
  sDayName <- paste0("day",day)
  
  #!beware that age currently reversed
  index <- nrow(lPop$dfRecordF) - age
  lPop$dfRecordF[[sDayName]][index] <- fPop
  
  return(list(lPop))
}

#eek this is getting tricky
#this might only return a list containing not all I want
#but it might work ...
lGrid2 <- laply(lGrid, rtSetF, fPop=1.14, day=1, age=1 )
dim(lGrid2) <- dim(lGrid)
#this shows that the above works
lGrid2[[1]][["dfRecordF"]][["day1"]][119]
#But I would want to pass a matrix of pop values, not just 1
#lGrid2 <- laply(lGrid, rtSetF, fPop=mGridFAge1, day=1, age=1 )
##number of items to replace is not a multiple of replacement length

#I want to keep this simpler
#currently for testing I'm using the popn record objects

#BUT the current day can be recorded differently
#this is also relevant to how I'm going to run the model
#for multiple grid cells

#?? might I be able to use an array, with dimensions for age,x & y
#well 4 arrays, F,M,pupF,pupM

nRow <- 10
nCol <- 10
nAge <- 7
#dimnames a list with one component for each dimension, NULL or a character vector of the length given by dim. 
#The list can be named, and the list names will be used as names for the dimensions.
dimnames <- list(NULL,NULL,NULL)
names(dimnames) <- c("x","y","age")
aF <- array(0, dim=c(nCol,nRow,nAge), dimnames=dimnames)
#adding 100 females of age 3 at 5,5
aF[5,5,3] <- 100
#to get a matrix for one age class
#cool
aF[,,3]
class(aF[,,3])
#[1] "matrix"
#cool so it could be displayed by raster
#also I can easily fill a constant age structure
aF[5,5,] <- 100
aF[5,5,]
#so can I move all age classes with a single line ?
#beware that I don't disallow dispersal rate changing with age & habitat

#moving a single age class
rtMove1(aF[,,1])
#can I move all age classes
alply(aF, .margins=c(1,2), function(x) rtMove1(aF[,,x]) )
#Error: incorrect number of dimensions
#ooooo this seems to do it
alply(aF, .margins=c(3), function(x) rtMove1(x) )
lOut <- alply(aF, .margins=c(3), function(x) rtMove1(x) )
#this does same with even less code
lOut <- alply(aF, .margins=3, rtMove1 )
#outputs a list with one matrix for each age
str(lOut)
#List of 7
#$ 1: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 2: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 3: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 4: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 5: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 6: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#$ 7: num [1:10, 1:10] 0 0 0 0 0 0 0 0 0 0 ...
#- attr(*, "split_type")= chr "array"
#- attr(*, "split_labels")='data.frame':  7 obs. of  1 variable:
#  ..$ age: Factor w/ 7 levels "1","2","3","4",..: 1 2 3 4 5 6 7

#creating stackoverflow question
nRow <- 10
nCol <- 10
nAge <- 7
#creating the array
dimnames <- list(NULL,NULL,NULL)
names(dimnames) <- c("x","y","age")
aF <- array(0, dim=c(nCol,nRow,nAge), dimnames=dimnames)

#aaply the identity function to the 3rd dimension
aTst <- aaply(aF, .margins=3, identity )
dim(aF)
#[1] 10 10  7
dim(aTst)
#[1]  7 10 10
#I'm aware it can be changed back using aperm but if I can avoid that it would be good.
aTst2 <- aperm(aTst, c(2, 3, 1))


