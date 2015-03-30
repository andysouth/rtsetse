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

#This follows hat-trick sheet:StabCalc c345
#It is tricky
 
#Effectively what it says is:
#If a stable popn of 1300 females produce 100 larvae. 
#If you want to get a carrying capacity of 4000 females you need to multiply 100 larvae by 4000/1300.  

  
#1 find number of age0 females produced from 100 larvae of both sexes   
fPopFAge0Ref <- 50*(1-pMortPupa)

#2 find stable age structure of females from this and age specific mortalities
vPopFRef <- rtSetAgeStructure(vpMortF, fPopAge0=fPopFAge0Ref)

#3 calculate total female population
fTotPopFRef <- sum(vPopFRef) 

#4 calculate a correction factor to scale up from the 100 larvae to get the desired carrying capacity
fCorrect4CarryCap <- (iCarryCapF / fTotPopFRef) * fStartPopPropCC   

#5 set the number of pupae using the correction
fPupaPerSexAge <- fPopFAge0Ref*fCorrect4CarryCap


fPupaPerSexAge
}