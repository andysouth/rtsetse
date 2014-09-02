#' testing seeking mortality that gives popn stability
#'
#' \code{rtMortalityStableSeek} 
#' early development

#' @param iMaxAge max age of fly allowed in model
#' @param fPopAge1 starting popn at age1
#' @param fInterval the interval between mortalities used to test
#' @param fMperF desired numbers of males per female, default=0.5
#' @param pMortLarva larval mortality per period
#' @param pMortPupa pupal mortality per period
#' 
#' #' @return float mortality probability
#' @export


rtMortalityStableSeek <- function( iMaxAge = 100,
                                  fPopAge1 = 100,
                                  fInterval = 0.001,
                                  fMperF = 0.5,
                                  pMortLarva = 0.05,
                                  pMortPupa = 0.25,
                                  plot =  TRUE )
{
  
  #create a vector of mortalities to test both M&F
  #starts low & increases
  #unlikely to ever go above mort 0.4 (or even get close to)
  vMorts <- seq(fInterval, 0.4, fInterval) 
  
  ########
  #Females
  
  #create a vector filled with NAs for results
  vLarvae <- rep(NA,length(vMorts))
  
  #set trialLarvae high to start the while loop
  numTrialLarvae <- fPopAge1+1
  trial <- 1
  #try increasing mortalities while trial is above threshold
  while( numTrialLarvae > fPopAge1 )
  {
    #set age1 mort for this trial
    fMort <- vMorts[trial]
    #set mortality by age (& determines num ages)
    #this is an early test, later mortality will be age dependent
    #todo: age-dependent mortality
    vpMort <- rep(fMort, iMaxAge)
    
    #fill an age structure
    #todo: decide whether to add pupal mort before or after here
    vPopF <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=fPopAge1 )
    
    #setting age dependent mortality rates
    vpDeposit <- rtSetDepositionRatesByAgeDI( iMaxAge=iMaxAge, pMortLarva=pMortLarva )
    #calc M&F larvae using the deposition rates set above
    lLarvae <- rtLarvalDeposition( vPopF, vpDeposit )    
    #summing M&F
    numTrialLarvae  <- lLarvae$iLarvaeF + lLarvae$iLarvaeM  
        
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
    abline(h=fPopAge1, col='red')    
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
    vPopM <- rtSetAgeStructure( vpMort=vpMort, fPopAge1=fPopAge1 )
    
    #sum num males in all age classes
    numTrialMales <- sum(vPopM)
    
    vMales[trial] <- numTrialMales
    trial <- trial+1
  }
  #set bestMort to the final trial (where larvae>=fPopAge)
  bestMortM <- vMorts[trial-1]
  cat("bestMort for males:",bestMortM,"males:",numTrialMales,"\n")
  
  lBestMorts <- list(F=bestMortF, M=bestMortM)
  
  lBestMorts
}