## ----global_options, include=FALSE---------------------------------------
#to try to stop figs being moved too much
#didn't seem to make any difference !
#possibly because it's non R figures I'm trying to position
#perhaps I can set an option in the markdown figure call ?
#http://stackoverflow.com/questions/29696172/how-to-hold-figure-position-with-figure-caption-in-pdf-output-of-knitr
knitr::opts_chunk$set(fig.pos = 'H')

## ---- eval=FALSE, echo=TRUE, message=FALSE-------------------------------
#  if(!require(devtools)) install.packages("devtools", dependencies=TRUE)
#  
#  install_github('AndySouth/rtsetse', build_vignettes=TRUE)

## ---- eval=FALSE, echo=TRUE, message=FALSE-------------------------------
#  # Load the package in each new R session.
#  library(rtsetse)

## ---- eval=FALSE, echo=TRUE, message=FALSE-------------------------------
#  # This will bring up a brief description of the rtsetse package :
#  ?rtsetse
#  
#  # Click on 'index' at the bottom of the description page to go to a list of help pages
#  # for each function in the package.
#  
#  # This document
#  vignette('rtsetse-workflow')
#  
#  # documents about movement and a model test
#  vignette('vignette-movement.pdf')
#  
#  vignette('vignette-eg1-sav-grass.pdf')
#  

## ---- eval=FALSE, echo=TRUE, message=FALSE-------------------------------
#  
#  # to start a graphical user interface
#  # with options and buttons allowing the model to be run and outputs to be viewed
#  rt_UI()

## ---- eval=FALSE, echo=TRUE, message=FALSE-------------------------------
#  
#  # To find documentation about this function and the inputs.
#  ?rt_runGrid
#  
#  # Click on 'index' at the bottom of the description page to go to a list of help pages
#  # for all other functions in the package.
#  
#  # Set number of days.
#  iDays <- 10
#  # Run the model (results are saved to aRecord or any othr object name you choose to use).
#  aRecord <-
#    rt_runGrid (  pMoveF=0.6,  pMoveM=0.3,   iDays=iDays,  pMortF=0.082,  pMortM=0.139,
#                  pMortPupa=0.2,  fStartPopPropCC=1,  iCarryCapF=200,  propMortAdultDD=0.25,
#                  iFirstLarva=16,  iInterLarva=10,  pMortLarva=0.05,  propMortLarvaDD=0.25,
#                  propMortPupaDD=0.25,
#                  mVegCats=system.file("extdata","vegHalfSavannahHalfGrass50x50.txt",
#                                       package="rtsetse"),
#                  dfMortByVeg=list(code = c("D", "T", "O", "S", "B", "G", "N"),
#                                   name = c("Dense Forest", "Thicket", "Open Forest",
#                                            "Savannah", "Bush", "Grass", "No-go area"),
#                                   mortality = c(200, 150, 110, 100, 110, 210, 999),
#                                   pupmortality = c(120, 110, 105, 100, 120, 170, 999))  )
#  
#  # Plot some of model results (in this case adult popn across the whole grid).
#  rtPlotPopGrid(aRecord)
#  # or plot a map of the population distribution
#  rtPlotMapPop(aRecord)
#  # or select particular days, ages and genders
#  rtPlotMapPop(aRecord, days='final', sex = 'M', age = (20:120))
#  

