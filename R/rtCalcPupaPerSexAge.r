#' calculating number of pupae in each age and sex class based on carrying capacity
#'
#' \code{rtCalcPupaPerSexAge} 
#' calculates the number of pupae in each and sex class

#' @param pMortPupa pupal mortality per period
#' @param vpMortF a vector of age specific F mortality rates, number of age classes is also got from this.
#' @param fStartPopPropCC starting population as a proportion of carrying capacity, default = 1
#' @param iCarryCapF carrying capacity of adult females 
#' 
#' @return a vector of abundance by age
#' @export

rtCalcPupaPerSexAge <- function( pMortPupa = 0.25, 
                                 vpMortF,
                                 fStartPopPropCC = 1,
                                 iCarryCapF = 200)
{


#to set num pupae per age class following hat-trick StabCalc c345
#50*(1-pMortPupa)*pupDurF*andyCorrect4CarCap 

#todo: refactor these nasty variable names
#1 create a reference popn that produces 100 larvae
fPopFAge0Ref <- 50*(1-pMortPupa)
vPopFRef <- rtSetAgeStructure(vpMortF, fPopAge0=fPopFAge0Ref)
fTotPopFRef <- sum(vPopFRef) 
#fCorrect4CarryCap <- iCarryCapF / fTotPopFRef 
#16/9/14 andy added ability to set starting popn by proportion of CC
#here just influences the pupae, this effects adult age structure later
fCorrect4CarryCap <- fStartPopPropCC * iCarryCapF / fTotPopFRef   

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setting pupae ################
#!BEWARE I'm not sure this is right yet
fPupaPerSexAge <- fPopFAge0Ref*fCorrect4CarryCap


fPupaPerSexAge
}