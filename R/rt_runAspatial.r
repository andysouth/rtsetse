#' an a-spatial tsetse population simulation, a third test of phase1
#'
#' \code{rt_runAspatial} runs an a-spatial popn simulation as a test of phase 1 model components.
#' This differs from previous tests in being closer to Hat-trick.
#' \cr ~ uses age-dependent mortalities.  
#' \cr ~ tries to start popn. at stable age structure.
#' \cr rt_runAspatial was developed from rtPhase1Test3

#' @param iDays days to run simulation
#' @param iMaxAge max age of fly allowed in model (will warn if flies age past this)
#' @param iCarryCapF carrying capacity of adult females 
#' @param fMperF numbers of males per female, default 0.5 for half as many M as F
#' @param fStartPopPropCC starting population as a proportion of carrying capacity, default = 1
#' @param iStartAdults number of adults to start simulation with
#' @param iStartAges spread start adults across the first n ages classes
#' @param iStartPupae number of pupae to start simulation with (they get spread across sex&age)
#' @param pMortF adult female mortality on day1, rates on later days are determined by following parameters.
#' @param pMortM adult male mortality on day1, rates on later days are determined by following parameters.
#' @param iMortMinAgeStart  Age at which min death rates start. 
#' @param iMortMinAgeStop   Age at which min death rates stop.
#' @param fMortMinProp  What proportion of the maximum death rate on day 0 is the minimum death rate.
#' @param fMortOldProp  What proportion of the maximum death rate on day 0 is the death rate after iDeathMinAgeStop.
#' @param propMortAdultDD proportion of adult mortality that is density dependent
#' @param pMortPupa pupal mortality per period
#' @param propMortPupaDD proportion of pupal mortality that is density dependent
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(M) to develop
#' @param iFirstLarva Age that female produces first larva
#' @param iInterLarva Inter-larval period
#' @param pMortLarva larval mortality per period
#' @param propMortLarvaDD proportion of larval mortality that is density dependent
#' @param plot whether to plot graphs
#' @param verbose whether to output progress messages to console 
#' 
#' @return a list of lots: 
#' \cr dataframes of adult age structure over time dfRecordF & M
#' \cr dataframes of adult age structure over time dfRecordPupa F & M
#' @examples
#' tst <- rt_runAspatial()
#' rtPlotPopAndPupae(tst$dfRecordF, tst$dfRecordM, tst$dfRecordPupaF, tst$dfRecordPupaM)
#' @export

rt_runAspatial <- function( iDays = 30,
                          iMaxAge = 120,
                          iCarryCapF = 200,
                          fMperF = 0.5,
                          fStartPopPropCC = 1,
                          iStartAdults = 200,
                          iStartAges = 1,
                          iStartPupae = 200,
                          pMortF = 0.05,
                          pMortM = 0.05,
                          iMortMinAgeStart = 10,
                          iMortMinAgeStop = 50,
                          fMortMinProp = 0.2,
                          fMortOldProp = 0.3,
                          propMortAdultDD = 0.25,
                          pMortPupa = 0.25,
                          propMortPupaDD = 0.25,
                          iPupDurF = 26,
                          iPupDurM = 28,
                          iFirstLarva = 16,
                          iInterLarva = 10,
                          pMortLarva = 0.05,
                          propMortLarvaDD = 0.25,
                          plot =  FALSE,
                          verbose = TRUE )
{
 
  #8/9/14 I want to modify this to start close to stable age structure
  #of both adults & pupae
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # setting mortality ################
  #constant by age
  #   vpMortF <- rep(pMortF,iMaxAge) 
  #   vpMortM <- rep(pMortM,iMaxAge) 
  
  #age dependeny mortality
  vpMortF <- rtSetMortRatesByAge( iMaxAge = iMaxAge, 
                                  pMortAge1 = pMortF,
                                  iMortMinAgeStart = iMortMinAgeStart,
                                  iMortMinAgeStop = iMortMinAgeStop,
                                  fMortMinProp = fMortMinProp,
                                  fMortOldProp = fMortOldProp )  
  vpMortM <- rtSetMortRatesByAge( iMaxAge = iMaxAge, 
                                  pMortAge1 = pMortM,
                                  iMortMinAgeStart = iMortMinAgeStart,
                                  iMortMinAgeStop = iMortMinAgeStop,
                                  fMortMinProp = fMortMinProp,
                                  fMortOldProp = fMortOldProp )   
  
  #setting a total carryCap from the female input
  #used later e.g. in mortality
  iCarryCap <- iCarryCapF * (1+fMperF)
  
  #calculating start numbers of pupae
  fPupaPerSexAge <- rtCalcPupaPerSexAge(pMortPupa = pMortPupa, 
                                        vpMortF = vpMortF,
                                        fStartPopPropCC = fStartPopPropCC,
                                        iCarryCapF = iCarryCapF)
    
  #vectors for pupae filled with same number of pupae at all ages
  #because males stay in the ground longer this means there will be more males 
  vPupaF <- rep(fPupaPerSexAge, iPupDurF)
  vPupaM <- rep(fPupaPerSexAge, iPupDurM)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # setting adults ################  
  #vectors for adults - start pop blank
  vPopStartF <- rep(0,iMaxAge)
  vPopStartM <- rep(0,iMaxAge)
  
  #add starting flies
  #2 params allow number and spread of flies across age classes to be set
#   vPopStartF[1:iStartAges] <- iStartAdults/(2*iStartAges)
#   vPopStartM[1:iStartAges] <- iStartAdults/(2*iStartAges)
  
  #start popn at stability
  #initialising age structure with the calc num pupae from above
  vPopStartF <- rtSetAgeStructure(vpMortF, fPopAge0=fPupaPerSexAge)
  vPopStartM <- rtSetAgeStructure(vpMortM, fPopAge0=fPupaPerSexAge)
  
  vPopF <- vPopStartF
  vPopM <- vPopStartM


  
  #storing age structure data
  #a datframe with one column per day & rows are ages
  dfRecordF <- data.frame(day0=rev(vPopF))
  dfRecordM <- data.frame(day0=rev(vPopM))
  #i'm not sure it's necessary to record pupal age stucture, 
  #but easier to do same way as adults
  dfRecordPupaF <- data.frame(day0=rev(vPupaF))
  dfRecordPupaM <- data.frame(day0=rev(vPupaM))  
  
  # start of day loop ################
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
    lPop <- rtMortality( vPopF, vPopM, vpMortF, vpMortM, 
                         propDD = propMortAdultDD,
                         iCarryCap = iCarryCap) 
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
    
    
    
    ###############
    ## fecundity ##
    #test1 all F set to 1
    #vpDeposit <- rep(1,length(vPopF))
    #test3 F>age5 set to 0.5
    #vpDeposit <- ifelse(seq(vPopF)>5,0.5,0)
    
    #set deposition rates by age
    vpDeposit <- rtSetDepositionRatesByAge( vPopF = vPopF,
                                            vPopM = vPopM,
                                            iFirstLarva = iFirstLarva,
                                            iInterLarva = iInterLarva,
                                            pMortLarva = pMortLarva,
                                            propMortDD = propMortLarvaDD,
                                            iCarryCap = iCarryCap)  
    #use the deposition rates set above
    lLarvae <- rtLarvalDeposition( vPopF, vpDeposit )    
    
    #Pupae at day1 set from deposited larvae
    #! be careful about where pupal ageing occurs
    vPupaF[1] <- lLarvae$fLarvaeF
    vPupaM[1] <- lLarvae$fLarvaeM
    
    ## pupal mortality ##
    #applied at day1 for the whole period
    #because of density dependence each sex needs to know about density of the other
    #iCarryCapPupa set from iCarryCap here because hat-trick default runs show similar numbers of ads & pupae at stability
    lPupae <- rtPupalMortality(vPupaF=vPupaF, 
                               vPupaM=vPupaM, 
                               pMort=pMortPupa, 
                               propDD=propMortPupaDD, 
                               iCarryCapPupa=iCarryCap )
    
    
    vPupaF <- lPupae$vPupaF
    vPupaM <- lPupae$vPupaM
      
    if ( verbose )
    {
      cat( "Larvae F: ",lLarvae$fLarvaeF,"  M: ",lLarvae$fLarvaeM,"\n\n")    
    }

    
    ## record age structure
    dfRecordF[,paste0("day",day)] <- rev(vPopF)
    dfRecordM[,paste0("day",day)] <- rev(vPopM)  
    dfRecordPupaF[,paste0("day",day)] <- rev(vPupaF)
    dfRecordPupaM[,paste0("day",day)] <- rev(vPupaM)      
  } #end of days loop
  
  
  #visualising changing age structure over time
  if (plot)
  {
    dev.new()
    rtPlotAgeStructure(dfRecordF,"Females")
    dev.new()
    rtPlotAgeStructure(dfRecordM,"Males")    
  }

  #returning the adult population record
  #! may want to change the structure of these outputs
  #maybe to a class
  invisible( list( dfRecordF = dfRecordF
                 , dfRecordM = dfRecordM
                 , dfRecordPupaF = dfRecordPupaF
                 , dfRecordPupaM = dfRecordPupaM))
  
  
  #popn pyramids
  #currently only work if maxAge for M&F has been set equal
  #library(pyramid)
  #pyramids(vPopF,vPopM)

}