#' testing seeking mortality that gives popn stability
#'
#' \code{rtMortalityStableSeek} 
#' early development

#' @param iMaxAge max age of fly allowed in model
#' @param iTargetPopAge0 target pop of both sexes at age 0
#' @param fInterval the interval between mortalities used to test
#' @param fMperF desired numbers of males per female, default=0.5
#' @param pMortLarva larval mortality per period
#' 
#' @param iMortMinAgeStart  Age at which min death rates start. 
#' @param iMortMinAgeStop   Age at which min death rates stop.
#' @param fMortMinProp  What proportion of the maximum death rate on day 0 is the minimum death rate.
#' @param fMortOldProp  What proportion of the maximum death rate on day 0 is the death rate after iDeathMinAgeStop.
#' @param propMortAdultDD proportion of adult mortality that is density dependent
#' @param pMortPupa pupal mortality per period
#' @param iPupDurF days it takes pupa(F) to develop
#' @param iPupDurM days it takes pupa(M) to develop
#' @param iFirstLarva Age that female produces first larva
#' @param iInterLarva Inter-larval period 
#' 
#' @param plot whether to output plots
#' @param verbose whether to output progress text
#' 
#' #' @return float mortality probability
#' @export


rtMortalityStableSeek <- function( iMaxAge = 100,
                                  iTargetPopAge0 = 100,
                                  fInterval = 0.001,
                                  fMperF = 0.5,
                                  pMortLarva = 0.05,
                                  
                                 iMortMinAgeStart = 10,
                                 iMortMinAgeStop = 50,
                                 fMortMinProp = 0.2,
                                 fMortOldProp = 0.3,
                                 propMortAdultDD = 0.25,
                                 
                                 pMortPupa = 0.25,
                                 iPupDurF = 26,
                                 iPupDurM = 28,
                                 iFirstLarva = 16,
                                 iInterLarva = 10, 
    
                                  plot = TRUE,
                                  verbose = FALSE )
{
  
  #create a vector of mortalities to test both M&F
  #starts low & increases
  #unlikely to ever go above mort 0.4 (or even get close to)
  vMorts <- seq(fInterval, 0.4, fInterval) 
  
  #impose pupal mort to get adults of age1 (division by 2 for both sexes)
  popAge1MorF <- iTargetPopAge0/2 * (1-pMortPupa) 
  #test with no pupal mort actually gave same results
  #popAge1MorF <- iTargetPopAge0/2  
  
  ########
  #Females
  
  #create a vector filled with NAs for results
  vLarvae <- rep(NA,length(vMorts))
  
  #set trialLarvae high to start the while loop
  numTrialLarvae <- popAge1MorF+1
  trial <- 1
  #try increasing mortalities while trial is above threshold
  while( numTrialLarvae > popAge1MorF )
  {
    #set age1 mort for this trial
    fMort <- vMorts[trial]
    #set mortality by age (& determines num ages)
    #this is an early test, later mortality will be age dependent
    #todo: age-dependent mortality
    vpMort <- rep(fMort, iMaxAge)
    
    #fill an age structure
    #todo: decide whether to add pupal mort before or after here
    vPopF <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=popAge1MorF )
    
    #setting age dependent mortality rates
    vpDeposit <- rtSetDepositionRatesByAgeDI( iMaxAge=iMaxAge, iFirstLarva=iFirstLarva, iInterLarva=iInterLarva, pMortLarva=pMortLarva )
    #calc M&F larvae using the deposition rates set above
    lLarvae <- rtLarvalDeposition( vPopF, vpDeposit )    
    #summing M&F
    numTrialLarvae  <- lLarvae$iLarvaeF + lLarvae$iLarvaeM  
    
    if (verbose) cat("trial MortF:",fMort,"totF:",sum(vPopF),"larvae:",numTrialLarvae,"\n")
    
    vLarvae[trial] <- numTrialLarvae
    trial <- trial+1
  }
  #set bestMort to the final trial (where larvae>=fPopAge)
  bestMortF <- vMorts[trial-1]
  cat("bestMortF:",bestMortF,"larvae:",numTrialLarvae,"\n")
  #calc tot F in best trial, used to calc num males below
  bestTotF <- sum(vPopF)
  
  if (plot)
  {
    plot( vMorts, vLarvae, type='l', ylab='larvae' )
    #add starting pop age1
    abline(h=popAge1MorF, col='red')    
  }
  
  
  ########
  #Males  
  
  
  #create a vector filled with NAs to save results
  vMales <- rep(NA,length(vMorts))
  
  
  #set males requested to total females * MFratio
  fMalesRequested <- bestTotF * fMperF
  
  #set trialLarvae high to start the while loop
  numTrialMales <- fMalesRequested + 1
  trial <- 1
  while( numTrialMales > fMalesRequested )
  {
    #set age1 mort for this trial
    fMort <- vMorts[trial]
    #set mortality by age (& determines num ages)
    #this is an early test, later mortality will be age dependent
    #todo: set age dependent mortality
    vpMort <- rep(fMort, iMaxAge)
    #fill an age structure
    #todo: decide whether to add pupal mort before or after here
    vPopM <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=popAge1MorF )
    
    #sum num males in all age classes
    numTrialMales <- sum(vPopM)
    
    if (verbose) cat("trial MortM:",fMort,"totM:",numTrialMales,"\n")
    
    vMales[trial] <- numTrialMales
    trial <- trial+1
  }
  #set bestMort to the final trial (where larvae>=fPopAge)
  bestMortM <- vMorts[trial-1]
  cat("bestMort for males:",bestMortM,"males:",numTrialMales,"\n")
  
  lBestMorts <- list(F=bestMortF, M=bestMortM)
  
  lBestMorts
}