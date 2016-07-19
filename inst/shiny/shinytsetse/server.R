#shinytse7/server.r
#andy south 15/09/2014


#to run type this in R console
#library(shiny)
#runApp('shinytse7')
#for me to deploy online
#devtools::install_github('AndySouth/rtsetse')
#devtools::install_github("rstudio/shinyapps")
#library(shinyapps)
#deployApp('shinytse7')

library(rtsetse)
library(shiny)
library(raster)


## can put functions here


#run the model once just one day before start to set up output structure
#output <- rtPhase1Test2(iDays=1, verbose=FALSE)
bestMorts <- rtMortalityStableSeek(plot=FALSE)
aspatialResults <- rt_runAspatial(iDays=1, verbose=FALSE)
lNamedArgsAspatial <- NULL #to hold argList for aspatial model  
gridResults <- rt_runGridTestSpread(nRow=1,nCol=1,iDays=1,report = NULL)
lNamedArgsGrid <- NULL #to hold argList for grid model  
stringCodeRepeat <- NULL # to hold the actual function call for the grid model, e.g. with filename for the veg matrix

shinyServer(function(input, output) {
#10/10/14 adding session arg for progress bar experiments
#shinyServer(function(input, output, session) {
  
  #I only want to run model when a button is pressed
  
  #v <- reactiveValues( output=output ) 
  v <- reactiveValues( bestMorts=bestMorts,
                       aspatialResults=aspatialResults,
                       gridResults=gridResults,
                       cachedTbl = NULL, 
                       dfRasterAtts = NULL) 
  
  #v$cachedTbl <- NULL   
  #load an example veg map to start, so that sim can be run without
  #having to load a map in the map tab first
  inFile <- system.file("extdata","veg_Example2x3.txt", package="rtsetse")
  v$cachedTbl <- read.table(inFile, as.is=TRUE) 
  
  #vegetation names & mortalities are read from a file later
  v$dfRasterAtts <- data.frame( code = c("D","T","O","S","B","G","N"), 
                              name = c("Dense Forest","Thicket","Open Forest","Savannah","Bush","Grass","No-go area"),
                              mortality = c(100,100,100,100,100,100,999),
                              pupmortality = c(100,100,100,100,100,100,999),
                              stringsAsFactors = FALSE )
  
  
  # run mortality seeking  ##########################
  runMortSeek <- reactive({
 
    cat("in runMortSeek button=",input$aButtonMortality,"\n")
    
    #changing from a submitButton to an actionButton
    #add dependency on the button
    if ( input$aButtonMortality == 0 ) return()
    #isolate reactivity of other objects
    isolate({
    
    #cat("in runMortSeek\n") #,input$days,"\n")
      
    v$bestMorts <- rtMortalityStableSeek( fMperF = input$fMperF,
                                       iPupDurF = input$iPupDurF,
                                       iPupDurM = input$iPupDurM, 

                                       iFirstLarva = input$iFirstLarva,
                                       iInterLarva = input$iInterLarva,
                                       
                                       pMortPupa = input$pMortPupa,
                                       pMortLarva = input$pMortLarva,    
                               
                                       iMortMinAgeStartF = input$iMortMinAgeStartF,
                                       #leave next as default
                                       #iMortMinAgeStopF = input$iMortMinAgeStopF,
                                       fMortMinPropF = input$fMortMinPropF,
                                       fMortOldPropF = input$fMortOldPropF,
                                       
                                       iMortMinAgeStartM = input$iMortMinAgeStartM,
                                       #leave next as default
                                       #iMortMinAgeStopM = input$iMortMinAgeStopM,
                                       fMortMinPropM = input$fMortMinPropM,
                                       fMortOldPropM = input$fMortOldPropM,
                                       
                                       
                                       #propMortLarvaDD = input$propMortLarvaDD,
                                       #propMortPupaDD = input$propMortPupaDD,
                                       #iStartAges = input$iStartAges,
                                       #iStartAdults = input$iStartAdults,  
                                       #iStartPupae = input$iStartPupae,   
                                       #iMaxAge = input$iMaxAge,
                                       verbose = FALSE )
    }) #end isolate            
  }) #end of runMortSeek

  
  # plot stability seeking  ##########################
  output$plotStableSeek <- renderPlot({
    
    #cat("in plotStableSeek input$fMperF=",input$fMperF,"\n")
    
    #needed to get plot to react when button is pressed
    #i'm not quite sure why, i thought it might react to v changing
    runMortSeek()
       
  })  
 
  # plot mortality by age F  ##########################
  output$plotMortalityF <- renderPlot({
    
    #?needed to get plot to react when button is pressed
    #runMortSeek()
    
    #!BEWARE that I have default max age set to 100 in a couple of places now. 
    #! ideally it should be set in just one place.
    iMaxAge <- 100
    iMortMinAgeStopF <- 60 #left as default, unchangeable in Hat-trick
    
    vpMorts <- rtSetMortRatesByAge( c(1:iMaxAge),
                                    pMortAge1 = v$bestMorts$F,
                                    iMortMinAgeStart = input$iMortMinAgeStartF,
                                    #leave next as default
                                    iMortMinAgeStop = iMortMinAgeStopF,
                                    fMortMinProp = input$fMortMinPropF,
                                    fMortOldProp = input$fMortOldPropF )
    
    rtPlotMortRatesByAge(vpMorts, title="females")
    
  })    
 
# plot mortality by age F  ##########################
output$plotMortalityM <- renderPlot({
  
  #?needed to get plot to react when button is pressed
  #runMortSeek()
  
  #!BEWARE that I have default max age set to 100 in a couple of places now. 
  #! ideally it should be set in just one place.
  iMaxAge <- 100
  iMortMinAgeStopM <- 40 #left as default, unchangeable in Hat-trick
  
  vpMorts <- rtSetMortRatesByAge( c(1:iMaxAge),
                                  pMortAge1 = v$bestMorts$M,
                                  iMortMinAgeStart = input$iMortMinAgeStartM,
                                  #leave next as default
                                  iMortMinAgeStop = iMortMinAgeStopM,
                                  fMortMinProp = input$fMortMinPropM,
                                  fMortOldProp = input$fMortOldPropM )
  
  
  rtPlotMortRatesByAge(vpMorts, title="males", col='blue')
  
})   
  
  ## FUNCTIONS used by aspatial tab   ###############################
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # run aspatial model   #######################
  runModel <- reactive({
    
    cat("in runModel button=",input$aButtonAspatial,"\n")
    
    #changing from a submitButton to an actionButton
    #add dependency on the button
    if ( input$aButtonAspatial == 0 ) return()
    #isolate reactivity of other objects
    isolate({
      
    
    if ( input$days > 0 )
    {
      
      #get mortalities from the stability calculation
      pMortF <- v$bestMorts$F
      pMortM <- v$bestMorts$M
      
      #put args into a global list (<<-) so they can also be printed elsewhere
      lNamedArgsAspatial <<- list(iDays = input$days,
                                  pMortF = pMortF,
                                  pMortM = pMortM, 
                                  iCarryCapF = input$iCarryCapF,
                                  fStartPopPropCC = input$fStartPopPropCC,
                                  propMortAdultDD = input$propMortAdultDD,
                                  #iMaxAge = input$iMaxAge,      
                                  propMortLarvaDD = input$propMortLarvaDD,
                                  propMortPupaDD = input$propMortPupaDD,
                                  #iStartAges = input$iStartAges,
                                  iStartAdults = input$iStartAdults, 
                                  
                                  #the params below are taken from page1
                                  iFirstLarva = input$iFirstLarva,
                                  iInterLarva = input$iInterLarva,
                                  pMortLarva = input$pMortLarva,  
                                  pMortPupa = input$pMortPupa,
                                  
                                  verbose=FALSE)

       #run the model with the list of args
       v$aspatialResults <- do.call(rt_runAspatial, lNamedArgsAspatial)

      #old way of doing before args were put into a list  
      #       v$aspatialResults <- rt_runAspatial(iDays = input$days,
      #                                         pMortF = pMortF,
      #                                         pMortM = pMortM, 
      #                                         propMortAdultDD = input$propMortAdultDD,
      #                                         iCarryCap = input$iCarryCap,
      #                                         #iMaxAge = input$iMaxAge,      
      #                                         propMortLarvaDD = input$propMortLarvaDD,
      #                                         propMortPupaDD = input$propMortPupaDD,
      #                                         #iStartAges = input$iStartAges,
      #                                         iStartAdults = input$iStartAdults, 
      #                                         
      #                                         #the params below are taken from page1
      #                                         iFirstLarva = input$iFirstLarva,
      #                                         iInterLarva = input$iInterLarva,
      #                                         pMortLarva = input$pMortLarva,  
      #                                         pMortPupa = input$pMortPupa,
      #                                         
      #                                         verbose=FALSE)
      
    } 
    }) #end isolate 
  })
  
  
  # plot total adult population  ##########################
  output$plotPop <- renderPlot({
    
    #needed to get plot to react when button is pressed
    runModel()
    
    cat("in plotPop input$days=",input$days,"\n")
    
    rtPlotPopAndPupae(v$aspatialResults$dfRecordF, v$aspatialResults$dfRecordM,
                      v$aspatialResults$dfRecordPupaF, v$aspatialResults$dfRecordPupaM)
    
  })  
  

  # print params used in aspatial model ###############################
  output$printParamsAspatial <- renderPrint({
    
    #needed to get plot to react when button is pressed
    runModel()
    
    cat("R code to repeat this run of the model locally using rtsetse version",
        packageDescription('rtsetse')$Version,
        "\n\n")    
    
    #Code to repeat this run of the model locally
    #copied from rtReportPhase2fromShiny
    sCommand <- "tst <- rt_runAspatial"
    #this creates a vector of 'name=value,'
    vArgs <- paste0(names(lNamedArgsAspatial),"=",lNamedArgsAspatial,", ")
    #to remove the final comma & space in args list
    vArgs[length(vArgs)] <- substr(vArgs[length(vArgs)],0,nchar(vArgs[length(vArgs)])-2)
    
    cat( sCommand,"( ",vArgs," )",sep="")
    
    cat( "\n\nrtPlotPopAndPupae(tst$dfRecordF, tst$dfRecordM, tst$dfRecordPupaF, tst$dfRecordPupaM)" )
    
  })    
   
  # plot female age structure   ###############################
  output$plotAgeStructF <- renderPlot({
        
    #needed to get plot to react when button is pressed
    #i'm not quite sure why, i thought it might react to v changing
    runModel()
    
    cat("in plotAgeStructF input$days=",input$days,"\n")
    
    rtPlotAgeStructure(v$aspatialResults$dfRecordF,"Females")
    
  })  

  # plot male age structure  ###############################
  output$plotAgeStructM <- renderPlot({
    
    #needed to get plot to react when button is pressed
    #i'm not quite sure why, i thought it might react to v changing
    runModel()
    
    cat("in plotAgeStructM input$days=",input$days,"\n")
    
    rtPlotAgeStructure(v$aspatialResults$dfRecordM,"Males")
    
  })    
  

  # plot mean age of adults  ###############################
  output$plotMeanAge <- renderPlot({
    
    runModel()
    
    cat("in plotMeanAge input$days=",input$days,"\n")
    
    rtPlotMeanAge(v$aspatialResults$dfRecordF, v$aspatialResults$dfRecordM,title="Mean age of adult flies")
        
  })    


## FUNCTIONS used by file loading tab   ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#these functions originally came from rtsetseMapEditor

# read input file, grid .txt and optional attribute .csv -----
readFileConductor <- reactive({ 

  if ( input$mapLocation=='Local')
  {
    if (is.null(input$fileMapLocal)) return(NULL)
    
    #if both grid(txt) & att(csv) files are loaded find the index
    #of which is the txt file
    indexOfGrid <- grep(".txt",input$fileMapLocal$name)
    #old way assumed their order
    #inFile <- input$fileMapLocal$datapath[ nrow(input$fileMapLocal) ]  
    inFile <- input$fileMapLocal$datapath[ indexOfGrid ]  
    
    cat("local grid file name =", input$fileMapLocal$name[ indexOfGrid ], "\n")   
    
  } else if ( input$mapLocation=='Internal')  
  {
    if (is.null(input$fileMapInternal)) return(NULL)
    #an internal file will be a character
    #inFile <- input$fileMapInternal
    #now that it is getting the file from the rtsetse package
    #ugly but does work BEWARE repeated below for attribute file
    inFile <- paste0( 'system.file("extdata","',input$fileMapInternal,'", package="rtsetse")')
    inFile <- eval(parse(text=inFile))
  }
  
  #reading in the grid.txt file as a character matrix
  #I may want to read in as a raster later so I can capture
  #cellsize and corner coords
  mat <- rtReadMapVeg(inFile) 
  
  #set the global var
  v$cachedTbl <<- mat
  
  
  ## now check for and load a raster attribute table
  ## not sure if should be in or outside of this function
  ## needs to cope with local & internal files

  
  if ( input$mapLocation=='Internal')
  {
    #in this case I can just check for the attribute table as a character
    #firstly just check for the text replaced with csv
    inFileAttributes <- sub(".txt",".csv",input$fileMapInternal)  
    #ugly but does work BEWARE repeated above for grid file
    inFileAttributes <- paste0( 'system.file("extdata","',inFileAttributes,'", package="rtsetse")')
    inFileAttributes <- eval(parse(text=inFileAttributes)) 
  }
  #if a local file it will be a dataframe and need to get datapath
  else if ( input$mapLocation=='Local') 
  {
    #one row indicates no attribute file
    if (nrow(input$fileMapLocal) == 1) inFileAttributes <- "no file"
    else {
      #this just converts 1to2 or 2to1
      #the attributes file is the one that the grid file wasn't
      indexOfAttributes <- ifelse(indexOfGrid==1,2,1)
      inFileAttributes <- input$fileMapLocal$datapath[indexOfAttributes]  
      
      cat("local attr file name =", input$fileMapLocal$name[ indexOfAttributes ], "\n")   
      
    }       
  }
 
  
  #check if the attributes file exists
  if( file.exists(inFileAttributes))
  {
    #read in the attribute file
    #note it is a csv file so needs read.csv
    dfAttributes <- read.csv(inFileAttributes, as.is=TRUE) 
    
    #TODO add checking for what's in the file
    #should be 7 rows with "D","T","O","S","B","G","N"
    #currently 3 columns code, name, mortality
    
    #set table from the file
    v$dfRasterAtts <<- dfAttributes
  } else
  { #if there is no attribute file, set from defaults
    warning("no raster attribute(.csv) file, setting default values")
    
    v$dfRasterAtts <- data.frame( code = c("D","T","O","S","B","G","N"), 
                                  name = c("Dense Forest","Thicket","Open Forest","Savannah","Bush","Grass","No-go area"),
                                  mortality = c(100,100,100,100,100,100,999),
                                  pupmortality = c(100,100,100,100,100,100,999),
                                  stringsAsFactors = FALSE )   
  }
  
})




### plot raster from a loaded text matrix of characters -----
output$plotLoadedMap <- renderPlot({
  
  #browser()
  #cat("in plotLoadedMap fileMap$datapath=",input$fileMap$datapath)
  
  if( is.null(input$fileMapInternal) & is.null(v$cachedTbl) ) return(NULL)
  else readFileConductor() #read from the inputFile if it hasn't been read yet
  #}
  
  
  #changing from a submitButton to an actionButton
  #add dependency on the button
  if ( input$aButtonMap < 0 ) return()
  #isolate reactivity of other objects
  isolate({
  
    mapMatrix <- as.matrix(v$cachedTbl)
    
    rtPlotMapVeg(mapMatrix, cex=1.2, labels=v$dfRasterAtts$name)
  
  }) #end isolate 
  
  #old way
  #all these steps do seem to be necessary to convert to a numeric matrix then raster
  #mapMatrix <- as.matrix(v$cachedTbl)
  #mapRaster <- raster(mapMatrix)    
  #set extents for plotting (otherwise they go from 0-1)
  #this also ensures that cells maintain square aspect ratio 
  #extent(mapRaster) <- extent(c(0, ncol(mapRaster), 0, nrow(mapRaster))) 
  #   plot(mapRaster)    
  
  #will spplot work with shiny ? YES
  #spplot(mapRaster)
    
}) #end of plotLoadedMap  


# table of raster attributes (vegetation) -----
output$tableRasterAtts <- renderTable({
  
  #create a test dataframe
#   dF <- data.frame( code = c("D","T"),
#                     name = c("Dense Forest","Thicket"),
#                     mortality = c(100,200)
#                     )
#   dF
  
  cat("in tableRasterAtts\n")
  
  v$dfRasterAtts
  
}) #end tableRasterAtts 


# editable raster attributes ######################
output$editableRasterAtts <- renderHtable({

  #browser()
  
  #if no changes have been made to the table
  #we might get here if a new file has been loaded
#   if ( is.null(input$editableRasterAtts) ) {  
#     
#     cat("in editableRasterAtts null\n")
#     
#   } else {
#     #save edited table changes 
#     cat("in editableRasterAtts saving changes\n",unlist(input$editableRasterAtts),"\n")
#     v$dfRasterAtts <<- input$editableRasterAtts
#   }

  #if the table has been changed from the UI, save those changes
  if ( !is.null(input$editableRasterAtts) ) {  

    #save edited table changes 
    cat("in editableRasterAtts saving changes\n",unlist(input$editableRasterAtts),"\n")
    v$dfRasterAtts <<- input$editableRasterAtts
  }
  
  v$dfRasterAtts
  
}) #end editableRasterAtts 
  

# table of inFile (not editable) -----
output$tableNonEdit <- renderTable({
  
  if( is.null(input$Internal) & is.null(v$cachedTbl) ) return(NULL)
  #else v$cachedTbl <<- readTxtChar() #read from the inputFile
  else readFileConductor() #read from the inputFile if it hasn't been read yet
  #}
  
  #     mapDF <- readTxtChar()
  #     mapDF
  v$cachedTbl
  
}) #end of tableNonEdit 


## FUNCTIONS used by grid tab   ###############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # prerun grid model (sort inputs)  ##########################  
  prerunGridModel <- reactive({
      
    #1. create a list of inputs to run model now  
    #2. create a string to enable user to run model later      
    
      #cat("in prerunGridModel button=",input$aButtonGrid,"\n")
      
      #changed from submitButton to actionButton, add dependency on the button
      #if ( input$aButtonGrid == 0 ) return()
      #23/1/15 above stopped running control before grid & caused invalid argument to unary operator
      #it may be useful later for now make dependency simpler
      input$aButtonGrid
      #23/1/15 adding dependency on new control button
      input$aButtonControl
      
      #isolate reactivity of other objects
      isolate({
        
        #to check if no map has been loaded
        #now I load a map by default, but still good to have this check
        if ( is.null(v$cachedTbl) ) stop("no vegetation map loaded, please go to the 'load map' tab first")
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #1. create a list of inputs to run model now  
        
        #cat("in runGridModel input$daysGridModel=",input$daysGridModel,"\n")
        
        #get mortalities from the stability calculation
        pMortF <- v$bestMorts$F
        pMortM <- v$bestMorts$M
        
        #put args into a global list (<<-) so they can also be printed elsewhere
        #!BEWARE have to make sure that these match the argnames for rt_runGrid
        #!otherwise they get missed out when the code string is produced 
        #!and the default values will be used
        lNamedArgsGrid <<- list(
          #nRow = input$nRow,
          #nCol = input$nCol,
          pMoveF = input$pMoveF,
          pMoveM = input$pMoveM,
          iDays = input$daysGridModel,
          pMortF = pMortF,
          pMortM = pMortM, 
          pMortPupa = input$pMortPupa,
          fStartPopPropCC = input$fStartPopPropCC,
          iCarryCapF = input$iCarryCapF,
          #iStartAges = input$iStartAges,
          #iStartAdults = input$iStartAdults )    
          propMortAdultDD = input$propMortAdultDD,
          #iMaxAge = input$iMaxAge,
          iFirstLarva = input$iFirstLarva,
          iInterLarva = input$iInterLarva,
          pMortLarva = input$pMortLarva,        
          propMortLarvaDD = input$propMortLarvaDD,
          propMortPupaDD = input$propMortPupaDD )                        
        #verbose=FALSE)
        
        #if testSpread checkbox is selected a different simulation is run
        #but difficulty that the args for the functions are different
        #as a workaround can use formals to get the args of a function
        
        if ( input$testSpread )
        {       
          #no need to modify lNamedArgsGrid
          lArgsToAdd <- NULL #this is just a placeholder
          
        } else
        {
          #just use those args that are in the arg list for rt_runGrid
          #!!BEWARE this is a hack !!
          lNamedArgsGrid <- lNamedArgsGrid[ which(names(lNamedArgsGrid) %in% names(formals("rt_runGrid")))]
          
          #add the matrix containing the vegetation to the arg list 
          lArgsToAdd <- list(mVegCats=as.matrix(v$cachedTbl),
                             dfMortByVeg=v$dfRasterAtts) 
          
          #Shiny reads in the map file to a matrix to display it.
          #I don't then really want to pass the filename to rtsetse to make it read it in again.
          #But I do want to put the filename into the code tab, to make the code reproducible 
          
          #if in control tab, add control arguments
          if ( input$selectedTab == "control")
          #if ( input$selectedTab == "grid")
          {
            cat("in control tab\n")  
            lControlArgs <- list(pControl=input$pControl,
                                 iControlBorder=input$iControlBorder)
            
            lArgsToAdd <- c(lArgsToAdd,lControlArgs)            
          }

          
          #!BEWARE necessary to assign first then globally assign after
          lNamedArgsGrid <- c(lNamedArgsGrid,lArgsToAdd) 
          lNamedArgsGrid <<- lNamedArgsGrid
          
        }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #2. create a string to enable user to run model later
      if ( input$testSpread )
      {    
        sCommand <- "tst <- rt_runGridTestSpread"  
        
      } else
      { 
        sCommand <- "tst <- rt_runGrid"
        
        #I didn't manage to get the matrix into a reproducible string
        #use the filenames instead
        if (input$mapLocation == 'Local')
        {  
          lNamedArgsGrid$mVegCats <- paste0('"',input$fileMapLocal$name,'"')
          
        }
        else if (input$mapLocation == 'Internal')
        {
          #lNamedArgsGrid$mVegetation <- paste0('"',input$fileMapInternal,'"')
          #TODO this code is repeated from above
          inFile <- paste0( 'system.file("extdata","',input$fileMapInternal,'", package="rtsetse")')
          lNamedArgsGrid$mVegCats <- inFile
        }
      }
           
      
      #this creates a vector of 'name=value,'
      vArgs <- paste0(names(lNamedArgsGrid),"=",lNamedArgsGrid,", ")   
      
      #to remove the final comma & space in args list
      vArgs[length(vArgs)] <- substr(vArgs[length(vArgs)],0,nchar(vArgs[length(vArgs)])-2)
      
      #cat( sCommand,"( ",vArgs," )",sep="")
      #23/12/14 put this into a global string so I can put it into the run report as well
      stringCodeRepeat <<- c( sCommand,"( ",vArgs," )")
      
        
      #cat("in prerunGridModel() created arg list, arg1:",unlist(lNamedArgsGrid)[1],"\n")    
      #cat("in prerunGridModel() created arg list:",unlist(lNamedArgsGrid),"\n")        
        
      }) #end isolate     
    }) # end prerunGridModel 

  # RUN grid model  ##########################  
  runGridModel <- reactive({
  
    #cat("in runGridModel lNamedArgsGrid=",unlist(lNamedArgsGrid),"\n")
    #cat("in runGridModel() created arg list, arg1:",unlist(lNamedArgsGrid)[1],"\n")  
    
    #cat("in runGridModel button=",input$aButtonGrid,"\n")    
    #changed from submitButton to actionButton, add dependency on the button
    #if ( input$aButtonGrid == 0 ) return()
    #23/1/15 above stopped running control before grid & caused invalid argument to unary operator
    #it may be useful later for now make dependency simpler
    input$aButtonGrid
    
    #23/1/15 adding dependency on new control button
    input$aButtonControl
    
    #experimenting with progress bar
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #this is the trickier way of doing via the API
    #because I may be able to pass the object to external functions
    progress <- shiny::Progress$new(min=1, max=15)
    on.exit(progress$close())   
    progress$set(message = 'Calculation in progress')#,
    #             detail = 'This may take a while...')
    progress$set(value = 15) #setting progress bar to max just to show it's doing something
    #ideally I might pass the progress object to the rtsetse functions that are taking the time
    
    
    #isolate reactivity of other objects
    isolate({    

      #tryCatch enables modification of the error msg that appears in shiny if memory runs out
      tryCatch({      
      
        #to test tryCatch
        #stop("test error msg")
        
        if ( input$testSpread )
        {
          v$gridResults <- do.call(rt_runGridTestSpread, lNamedArgsGrid)
        }else
        {       
          #cat("in runGridModel() calling rt_runGrid with args:",unlist(lNamedArgsGrid),"\n")
          
          v$gridResults <- do.call(rt_runGrid, lNamedArgsGrid)       
        }     
    
    },error = function(c) {
                           c$message <- paste0("probably lack of memory, try to rerun locally using code from the code tab.\n",c$message)
                           stop(c) 
                           }) #end tryCatch

    }) #end isolate   
  }) # end runGridModel 
  
  
  
  # plotting pop maps for MF ###############################
  output$plotMapDays <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in plotMapDays input$daysGridModel=",input$daysGridModel,"\n")
    
    rtPlotMapPop(v$gridResults, days='all', ifManyDays = 'spread', sex='MF')
  })  

# plotting controlled pop maps for MF ###############################
# currently identical to plotMapDays except checks aButtonControl
output$plotMapDaysControl <- renderPlot({
  
  #only try to display results after the run button has been pressed for the first time
  if ( input$aButtonControl == 0 ) return( msgRunPrompt() )
  
  #needed to get plot to react when button is pressed
  prerunGridModel()
  runGridModel()
  
  #with this in a display refresh is triggered when days are changed
  #cat("in plotMapDays input$daysGridModel=",input$daysGridModel,"\n")
  
  rtPlotMapPop(v$gridResults, days='all', ifManyDays = 'spread', sex='MF')
})  

  # message to prompt user to press run --------------
  #options for renderType are 'plot' 'print' and 'text'
  msgRunPrompt <- function( renderType="plot", ... )
  {
    #... allows other mtext params to be passed
    msg <- "press the 'Run Model' button to see output here"
    
    if (renderType=="plot")
      mtext(msg, col='blue', ...)
    else if (renderType=="print")
      cat(msg)
    else #text
      msg
  }


  # plotting pop maps for F ###############################
  output$plotMapDaysF <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    rtPlotMapPop(v$gridResults, days='all', ifManyDays = 'spread', sex='F')
  })  

  # plotting pop maps for M ###############################
  output$plotMapDaysM <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    rtPlotMapPop(v$gridResults, days='all', ifManyDays = 'spread', sex='M')
  }) 
  
  
  # plot pop map for final day ###############################
  output$plotMapFinalDay <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in plotMapFinalDay input$daysGridModel=",input$daysGridModel,"\n")
    
    rtPlotMapPop(v$gridResults, days='final', sex='MF')
    
    
  })  
  
  
  # plot adult popn & M&F for whole grid ###############################
  output$plotPopGrid <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in plotPopGrid input$daysGridModel=",input$daysGridModel,"\n")
    
    rtPlotPopGrid(v$gridResults,"Adults") 
    #print( rtPlotPopGrid(v$gridResults,"Adult Flies") )
    
    
  })  
  
  
  # plot mean age of adults ###############################
  output$plotMeanAgeGrid <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in plotMeanAgeGrid input$daysGridModel=",input$daysGridModel,"\n")
    
    rtPlotMeanAgeGrid(v$gridResults)
    
  })  
  
  
  # download a report #########################################
  # code from: http://shiny.rstudio.com/gallery/download-knitr-reports.html
  # the report format is set by a Rmd file in the shiny app folder
  # note this doesn't use the reporting function from rtsetse
  output$downloadReport <- downloadHandler(
    
    #this was how to allow user to choose file
    # filename = function() {
    #     paste('rtsetseReport', sep = '.', switch(
    #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #     ))
    #   },
    
    # name of the report file to create
    filename = "rtsetseReport.pdf",
    
    content = function(file) {
      
      #name of the Rmd file that sets what's in the report
      filenameRmd <- 'rtReportPhase2fromShinytse7.Rmd'
      
      src <- normalizePath(filenameRmd)
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, filenameRmd, overwrite=TRUE)
      
      library(rmarkdown)
      
      #this allowed rendering in pdf,html or doc
      # out <- render(filenameRmd, switch(
      #   input$format,
      #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
      # ))
      
      #rendering in html only
      #out <- render(filenameRmd, html_document())    
      
      #rendering in pdf only
      out <- render(filenameRmd, pdf_document())   
      
      
      file.rename(out, file)
    }
  ) #end downloadReport

  
  # save grid results to a text file #########################################
  # I had problems with this not working when run from RStudio viewer
  # this thread points out that I may be better just allowing user to save locally (& assume they are running in an R process on their local machine)
  # https://groups.google.com/forum/#!searchin/shiny-discuss/download/shiny-discuss/PB57zk7Cbto/dTsn-2r7xSUJ
  # for now keep this & instruct to run app in browser
  output$saveResultsGrid <- downloadHandler(
    
    filename = function() { 
      paste("rtsetseResults.txt") 
      #paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      rtWriteResults(v$gridResults, file=file, age="sum", sex="all")
      # write.csv(data.frame( country=c("Spain", "United Kingdom"),
      #                       weather=c("hot", "cold") ), file)
    } 
    
    # simple example from RStudio
    # filename = function() { 
    #   paste(input$dataset, '.csv', sep='') 
    # },
    # content = function(file) {
    #   write.csv(datasetInput(), file)
    # } 
    
  ) #end saveResultsGrid
    
  
  # test plotting of inputs ###############################
  #not currently used
  output$testInputs <- renderText({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in testInputs() input$daysGridModel=",input$daysGridModel,"\n")
    
    #this gets all of the reactive values
    #problem with that is that not all of them apply to the grid model
    lNamedArgs <- isolate(reactiveValuesToList(input))
    
    #names(lNamedArgs)[ names(lNamedArgs)!='iStartAges' ]
    #below works to omit 2 sets of vars, can use similar code in the report Rmd
    names(lNamedArgs)[ substring(names(lNamedArgs),1,2)!='iS' & substring(names(lNamedArgs),1,2)!='pM' ]
    
  })  
  
  
  # plot age struct summed M&F whole grid ###############################
  output$plotAgeStructGrid <- renderPlot({
    
    #only try to display results after the run button has been pressed for the first time
    if ( input$aButtonGrid == 0 ) return( msgRunPrompt() )
    
    #needed to get plot to react when button is pressed
    prerunGridModel()
    runGridModel()
    
    #with this in a display refresh is triggered when days are changed
    #cat("in plotAgeStructGrid input$daysGridModel=",input$daysGridModel,"\n")
    
    rtPlotAgeStructure(v$gridResults,"M & F summed for whole grid")
    
  })  

# print reproducible code for grid model ###############################
output$printParamsGrid <- renderPrint({
  
  #only try to display results after the run button has been pressed for the first time
  if ( input$aButtonGrid == 0 ) return( msgRunPrompt(renderType="print") )
  
  #this may allow correct code to be displayed when model has crashed ..
  prerunGridModel()
  #runGridModel()
  
  cat("R code to repeat this run of the model locally using rtsetse version",
      packageDescription('rtsetse')$Version,
      "\n\n")    
  
  
  #different function is run if the test spread checkbox is selected
  #only need to output extra text for the non-spread option
  if ( !input$testSpread )
  {
    
    if (input$mapLocation == 'Local')
    {  
      
      cat("As you are using a local vegetation file you will first need to use setwd() ",
          "to set your working directory to the location of the files.",
          "\n\n")  
      
    }
    #else if (input$mapLocation == 'Internal')
        
  }
  
  #this outputs it to the code tab
  #stringCodeRepeat is a global variable
  cat( stringCodeRepeat )
    
  cat( "\n\n#to plot some results \nrtPlotMapPop(tst)" )
  
})    

# display values of input for testing ###############################
output$testInputVals <- renderText({

  #only try to display results after the run button has been pressed for the first time
  if ( input$aButtonGrid == 0 ) return( msgRunPrompt(renderType="text") )
  
  #needed to get plot to react when button is pressed
  prerunGridModel()
  #runGridModel()
  
  #browser()
  lNamedArgs <- isolate(reactiveValuesToList(input))   
  namedArgs <- unlist(lNamedArgs) 
  paste0(names(namedArgs),"=",namedArgs)
  
  
}) #end of testInputVals 

  
}) # end of shinyServer()
