#' an a-spatial tsetse population simulation, a test of phase1
#'
#' \code{rtPhase1Test} runs an a-spatial popn simulation as a test of phase 1
#' model components. It uses default parameter values that are mostly similar to hat-trick.

#' @param iDays days to run simulation
#' @param iMaxAge max age of fly allowed in model (will warn if flies age past this)
#' @param iCarryCap carrying capacity of adults 
#' @param iStartAdults number of adults to start simulation with
#' @param iStartAges spread start adults across the first n ages classes
#' @param pMortF adult female mortality per day 
#' @param pMortM adult male mortality per day 
#' @param pMortPupa pupal mortality per period
#' @param propMortPupaDD proportion of pupal mortality that is density dependent
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(F) to develop
#' @param iFirstLarva Age that female produces first larva
#' @param iInterLarva Inter-larval period
#' @param pMortLarva larval mortality per period
#' @param plot whether to plot graphs
#' @param verbose whether to output progress messages to console 
#' 
#' @return a list of lots, initially dataframes of adult age structure over time dfRecordF & M
#' @examples
#' rtPhase1Test()
#' @export

rtPhase1Test <- function( iDays = 30,
                          iMaxAge = 120,
                          iCarryCap = 200,
                          iStartAdults = 200,
                          iStartAges = 1,
                          pMortF = 0.05,
                          pMortM = 0.05,
                          pMortPupa = 0.25,
                          propMortPupaDD = 0.25,
                          iPupDurF = 26,
                          iPupDurM = 28,
                          iFirstLarva = 16,
                          iInterLarva = 10,
                          pMortLarva = 0.05,
                          plot =  FALSE,
                          verbose = TRUE )
{

  #numMperF <- 1
  
  #vectors for death rates for males & females
  vpMortF <- rep(pMortF,iMaxAge) 
  vpMortM <- rep(pMortM,iMaxAge) 
  
  #vectors for pupae - empty to start
  vPupaF <- rep(0,iPupDurF)
  vPupaM <- rep(0,iPupDurM)
  
  #vectors for adults
  #start pop blank
  vPopStartF <- rep(0,iMaxAge)
  vPopStartM <- rep(0,iMaxAge)
  
  #add starting flies
  #2 params allow number and spread of flies across age classes to be set
  vPopStartF[1:iStartAges] <- iStartAdults/(2*iStartAges)
  vPopStartM[1:iStartAges] <- iStartAdults/(2*iStartAges)
  
  
  vPopF <- vPopStartF
  vPopM <- vPopStartM
  
  #?how might I store age structure data
  #could have a datframe with one column per day & rows are ages
  dfRecordF <- data.frame(day0=rev(vPopF))
  dfRecordM <- data.frame(day0=rev(vPopM))
  
  for( day in 1:iDays ) {
    
    #browser()
    if (verbose)
    {
      cat("day",day,"\nF: ",round(vPopF,1),"\nM: ",round(vPopM,1),"\n")
      cat("pupaF: ",round(vPupaF,1),"\npupaM: ",round(vPupaM,1),"\n")        
    }
    
    
    ###############
    ## mortality ##
    #!beware of potential bug of passing vectors in wrong order 
    #(remember ladies first) or do it a different way
    lPop <- rtMortality( vPopF, vPopM, vpMortF, vpMortM )
    vPopF <- lPop$vFem
    vPopM <- lPop$vMal  
  
    
    ##################
    ## adult ageing ##
    #(just moving adult flies into the next age class)
    vPopF <- rtAgeing(vPopF, label="adult F")
    vPopM <- rtAgeing(vPopM, label="adult M")
    
    
    #####################
    ## pupal emergence ##
    #! I don't think this is a partic good way of doing - look at
    l <- rtPupalEmergence( vPupaF, vPupaM, vPopF, vPopM )
    vPupaF <- l$vPupaF 
    vPupaM <- l$vPupaM
    vPopF <- l$vPopF
    vPopM <- l$vPopM
    
    
    ## pupal ageing ##
    vPupaF <- rtAgeing(vPupaF, label="pupae F")
    vPupaM <- rtAgeing(vPupaM, label="pupae M")
    
    
    #temporary test setting emerged pupae directly from deposited larvae
    #vPopF[1] <- iLarvaeF
    #vPopM[1] <- iLarvaeM
    
    ###############
    ## fecundity ##
    #test1 all F set to 1
    #vpDeposit <- rep(1,length(vPopF))
    #test3 F>age5 set to 0.5
    #vpDeposit <- ifelse(seq(vPopF)>5,0.5,0)
    
    #set deposition rates by age
    vpDeposit <- rtSetDepositionRatesByAge( vPopF, iFirstLarva, iInterLarva, pMortLarva )  
    #use the deposition rates set above
    lLarvae <- rtLarvalDeposition( vPopF, vpDeposit )    
    #! an ugly way of doing I can improve
    iLarvaeF <- lLarvae$iLarvaeF
    iLarvaeM <- lLarvae$iLarvaeM  
    
    #Pupae at day1 set from deposited larvae
    #! be careful about where pupal ageing occurs
    vPupaF[1] <- iLarvaeF
    vPupaM[1] <- iLarvaeM
    
    ## pupal mortality ##
    # is applied at day1 for the whole period
    #!arg now that I've implemented density dependence each sex needs to know about
    #?the abundance of the other, I might want to change the way I do this
    #vPupaF <- rtPupalMortality(vPupaF, pMortPupa)
    #vPupaM <- rtPupalMortality(vPupaM, pMortPupa)  
    #iCCPupa set from iCarryCap here because hat-trick default runs show similar numbers of ads & pupae at stability
    lPupae <- rtPupalMortality(vPupaF=vPupaF, vPupaM=vPupaM, pMort=pMortPupa, propDD=propMortPupaDD, iCCPupa=iCarryCap )
    vPupaF <- lPupae$vPupaF
    vPupaM <- lPupae$vPupaM
      
    if ( verbose )
    {
      cat( "Larvae F: ",iLarvaeF,"  M: ",iLarvaeM,"\n\n")      
    }

    
    ## record age structure
    dfRecordF[,paste0("day",day)] <- rev(vPopF)
    dfRecordM[,paste0("day",day)] <- rev(vPopM)  
  } #end of days loop
  
  
  #visualising changing age structure over time
  if (plot)
  {
    x11()
    rtPlotAgeStructure(dfRecordF,"Females")
    x11()
    rtPlotAgeStructure(dfRecordM,"Males")    
  }

  #returning the adult population record
  #! may want to change the structure of these outputs
  #maybe to a class
  invisible( list( dfRecordF=dfRecordF, dfRecordM = dfRecordM ))
  
  
  #popn pyramids
  #currently only work if maxAge for M&F has been set equal
  #library(pyramid)
  #pyramids(vPopF,vPopM)

}