#' tsetse larval deposition
#'
#' \code{rtLarvalDeposition} returns the number of resulting pupa male and female
#' 
#' from the age structure of adult females
#' and 'birth' probabilities per female.  
#' It used to round the number of larvae down to an integer, now allows fractional larvae. 
#' 
#' See \code{\link{rtLarvalDepositionGrid}} for the grid version.


#' @param vFem a vector of the age distribution of Females 
#' @param vpDeposit a vector of age-specific deposition probabilities of Females 
#' 
#' @return a list containing numbers of larvae: iLarvaeF, iLarvaeM
#' @export

rtLarvalDeposition <- function( vFem                                
                              , vpDeposit )
{
  
  #simply multiply the number of females in each age class by
  #the proportion depositing a larva
  
  #? should this be rounded to an integer for each age class ?
  
  vLarvae <- vFem * vpDeposit
  #calc total pupae (as a float)
  fLarvae <- sum(vLarvae)
  
  #assign gender and it used to round down to integer here
  #iLarvaeF <- floor(fLarvae/2)
  #iLarvaeM <- floor(fLarvae/2)  
  
  #divide between genders now allow fractional larvae
  fLarvaeF <- fLarvaeM <- fLarvae/2
  
  #it would be easy to make the model probabilistic by assigning gender with a 0.5 probability
  
  #return
  invisible( list(fLarvaeF=fLarvaeF, fLarvaeM=fLarvaeM) )
  
} #end of rtLarvalDeposition