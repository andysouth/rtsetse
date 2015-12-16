## ---- eval=TRUE, echo=TRUE, message=FALSE--------------------------------
#require(devtools)    
#install_github('AndySouth/rtsetse', build_vignettes=TRUE)
require(rtsetse)
#these packages are used for plots in this document
require(raster)
require(sp)
require(RColorBrewer)

## ---- "movement-1", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7-----

#set dimensions of the spatial grid
nY <- nX <- 5
nDays <- 3
#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))
#populate central cell of starting grid on day1
a[3,3,1] <- 1

#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove)  
}

#quick way of displaying population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

#repeat with lower movement proportion
pMove <- 0.4

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove)  
}

#quick way of displaying population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-2", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7-----

#set dimensions of the spatial grid
nY <- 5
nX <- 4
nDays <- 12
#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))

#populate central cell of starting grid on day1
a[3,3,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove)    
}

#quick way of displaying population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "movement-3", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=2----

nY <- 5
nX <- 4
nDays <- 5

#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))

#creating empty no-go matrix
mNog <- array(1, dim=c(nY,nX))
#set first row to nogo (0)
mNog[1,] <- 0

#populate central cell of starting grid on day1
a[3,3,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove, mNog=mNog)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mNog=mNog)    
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "movement-4", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7, fig.height=2----

#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))

#creating empty vegetation matrix
mVegMove <- array(1, dim=c(nY,nX))
#set 2nd row to lower movement
mVegMove[2,] <- 0.8

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "movement-4-2", eval=TRUE, echo=FALSE, message=FALSE, fig.width=7, fig.height=2----

#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))

#creating empty vegetation matrix
mVegMove <- array(1, dim=c(nY,nX))
#set 2nd row to higher movement
mVegMove[2,] <- 1.2

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "movement-4-3", eval=TRUE, echo=FALSE, message=FALSE, fig.width=7, fig.height=2----
#create array to store grids over time
a <- array(0, dim=c(nY, nX, nDays))

#creating empty vegetation matrix
mVegMove <- array(1, dim=c(nY,nX))
#set 2nd row to higher movement
mVegMove[2,] <- 1.2
#set 4th row to lower movement
mVegMove[4,] <- 0.8

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "load-and-plot-serengeti", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7----

#get the vegetation map
mVegCats <- rtReadMapVeg( system.file("extdata","vegTanzaniaSerengetiTorr1km.txt", package="rtsetse"))
#plot veg map
rtPlotMapVeg(mVegCats)


## ---- "movement-serengeti1-veg", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7----

#convert vegetation categories to movement multiplier values
dfMoveByVeg <-  data.frame(code=c("D","T","O","S","B","G","N"),move=c(0.85, 0.9, 0.95, 1, 1.05, 1.1, 0))
mVegMove <- rtSetGridFromVeg( mVegetation=mVegCats, dfLookup=dfMoveByVeg )

nDays <- 12

#create array to store grids over time, set dimensions from the veg grid
a <- array(0, dim=c(dim(mVegMove), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVeg(a[,,day-1], pMove=pMove, mVegMove=mVegMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegMove=mVegMove) 
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


#to look more closely at the final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-decreasing-check1", eval=TRUE, echo=TRUE, message=FALSE, fig.width=3, fig.show='hold'----
#create very simple test veg map
nY <- 5
nX <- 4
# G for Grass, S for Savannah
#vertical split
#m2 <- array("G", dim=c(nY,nX))
#m2[1:(nY*nX/2)] <- "S"
#horizontal split
m2 <- array("G", dim=c(nY,nX))
m2[1:round(nY/2),] <- "S"

nDays <- 99

#create array to store grids over time, set dimensions from the veg grid
a <- array(0, dim=c(dim(m2), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8
#set preferred vegetation type to Savannah(index=4)
iBestVeg <- 4

for(day in 2:nDays)
{
  #because mVegMove is not specified here, only the vegetation difference effect operates
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=m2)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=m2, iBestVeg=iBestVeg)  
}

#plot vegetation map
rtPlotMapVeg(m2)

#plot final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-decreasing-check2", eval=TRUE, echo=TRUE, message=FALSE, fig.width=3, fig.show='hold'----
#create very simple test veg map
nY <- 6
nX <- 4

#create horiz stripes of habitat
m2 <- array(c("D","T","O","S","B","G"), dim=c(nY,nX))

#m2[1:round(nY/2),] <- "S"

nDays <- 12

#create array to store grids over time, set dimensions from the veg grid
a <- array(0, dim=c(dim(m2), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.6

for(day in 2:nDays)
{
  #because mVegMove is not specified here, only the vegetation difference effect operates
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=m2)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=m2, iBestVeg=iBestVeg)   
}

#plot vegetation map
rtPlotMapVeg(m2)

#plot final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-decreasing-check3", eval=TRUE, echo=TRUE, message=FALSE, fig.width=3, fig.show='hold'----
#create very simple test veg map
nY <- 6
nX <- 4

#create horiz stripes of habitat
#m2 <- array(c("D","T","O","S","B","G"), dim=c(nY,nX))
#change order
m2 <- array(c("O","T","D","S","G","B"), dim=c(nY,nX))

nDays <- 12

#create array to store grids over time, set dimensions from the veg grid
a <- array(0, dim=c(dim(m2), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.6

for(day in 2:nDays)
{
  #because mVegMove is not specified here, only the vegetation difference effect operates
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=m2) 
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=m2, iBestVeg=iBestVeg)  
}

#plot vegetation map
rtPlotMapVeg(m2)

#plot final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-serengeti2-boundaries", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7----

nDays <- 12

#create array to store grids over time, set dimensions from the veg grid
a <- array(0, dim=c(dim(mVegCats), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #because mVegMove is not specified here, only the vegetation difference effect operates
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=mVegCats)
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=mVegCats, iBestVeg=iBestVeg )  
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


#to look more closely at the final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-serengeti3-boundary-check", eval=TRUE, echo=TRUE, message=FALSE, fig.width=3, fig.show='hold'----
#get the vegetation map
mVegCats <- rtReadMapVeg( system.file("extdata","vegTanzaniaSerengetiTorr1km.txt", package="rtsetse"))
#subset SE corner
m2 <- mVegCats[40:50,40:50]

nDays <- 99

#create array to store grids over time
#set dimensions from the veg grid
a <- array(0, dim=c(dim(m2), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #because mVegMove is not specified here, only the vegetation boundary effect operates
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=m2)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=m2, iBestVeg=iBestVeg )  
  }

#plot vegetation map
rtPlotMapVeg(m2)

#plot final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


## ---- "movement-serengeti4-both-veg-effects", eval=TRUE, echo=TRUE, message=FALSE, fig.width=7----

nDays <- 12

#create array to store grids over time
#set dimensions from the veg grid
a <- array(0, dim=c(dim(mVegCats), nDays))

#fill starting grid on day1
a[,,1] <- 1
#set proportion moving
pMove <- 0.8

for(day in 2:nDays)
{
  #a[,,day] <- rtMoveReflectNoGoVegBoundary(a[,,day-1], pMove=pMove, mVegCats=mVegCats, mVegMove=mVegMove)  
  a[,,day] <- rtMove(a[,,day-1], pMove=pMove, mVegCats=mVegCats, mVegMove=mVegMove)  
}

#display population spread over time
spplot( raster::brick(a, xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",1:nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )

## ---- "movement-serengeti-final-comparison", eval=TRUE, echo=TRUE, message=FALSE, fig.width=3, fig.show='hold'----

#plot vegetation map
rtPlotMapVeg(mVegCats)

#plot final day
spplot( raster::raster(a[,,nDays], xmx=ncol(a), ymx=nrow(a)), 
        names.attr=c(paste0("day",nDays)), 
        col.regions=c("white",colorRampPalette(brewer.pal(9,"Greens"))(99)) )


