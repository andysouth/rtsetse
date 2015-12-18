## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#install & load packages as needed, not echoed in this doc
#require(devtools)    
#install_github('AndySouth/rtsetse', build_vignettes=TRUE)
require(rtsetse)
#these packages are used for plots in this document
# require(raster)
# require(sp)
# require(RColorBrewer)

## ---- eval=TRUE, echo=FALSE, message=FALSE, fig.width=4, fig.height=2----
mVegCats <- rtReadMapVeg( system.file("extdata","vegHalfSavannahHalfGrass50x50.txt", package="rtsetse"))
rtPlotMapVeg(mVegCats)

## ---- eval=FALSE, echo=FALSE, message=FALSE------------------------------
#  #to run simulation
#  #not evaluated because takes ~1hr
#  #already done & saved in /data
#  iDays <- 150
#  aRecord <- rt_runGrid (  pMoveF=0.6,  pMoveM=0.3,   iDays=iDays,  pMortF=0.082,  pMortM=0.139,  pMortPupa=0.2,  fStartPopPropCC=1,  iCarryCapF=200,  propMortAdultDD=0.25,  iFirstLarva=16,  iInterLarva=10,  pMortLarva=0.05,  propMortLarvaDD=0.25,  propMortPupaDD=0.25,  mVegCats =system.file("extdata","vegHalfSavannahHalfGrass50x50.txt", package="rtsetse"),  dfMortByVeg=list(code = c("D", "T", "O", "S", "B", "G", "N"), name = c("Dense Forest", "Thicket", "Open Forest", "Savannah", "Bush", "Grass", "No-go area"), mortality = c(200, 150, 110, 100, 110, 210, 999), pupmortality = c(120, 110, 105, 100, 120, 170, 999))  )

## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
#load sim, no need to echo
#may just lazyLoad so may not need this

## ---- eval=TRUE, echo=FALSE, message=FALSE, fig.width=5.7, fig.height=2.9, fig.align='left'----
data(aRecord)

days2plot <- c(1,10,20,40,60,80,100,120,150)

#fMaxCellVal set to 300 so that all plots use same colour scheme
#ext=c(10,40,10,40) to zoom in
tmp <- lapply(days2plot, function(x) rtPlotMapPop(aRecord, sex='M&F', days=x, fMaxCellVal=300, ext=c(10,40,10,40)))


## ---- eval=TRUE, echo=FALSE, message=FALSE, fig.width=5.7, fig.height=2.9, fig.align='left'----

days2plot <- c(1,10,20,40,60,80,100,120,150)

#fMaxCellVal set to 300 so that all plots use same colour scheme
#ext=c(10,40,10,40) to zoom in
tmp <- lapply(days2plot, function(x) rtPlotMapPop(aRecord, sex='M&F', days=x, ext=c(10,40,10,40), age=c(20:120)))


## ---- echo=FALSE---------------------------------------------------------
rtPlotPopGrid(aRecord)

## ---- echo=FALSE---------------------------------------------------------
rtPlotPopGrid(rtGetFromRecord(aRecord, age=c(20:120)))

