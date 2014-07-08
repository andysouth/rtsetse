#' tsetse larval deposition
#'
#' \code{rtLarvalDeposition} returns the number of resulting pupa male and female
#' 
#' from the age structure of adult females
#' and 'birth' probabilities per female.  
#' BEWARE that this round the number of larvae down to an integer. WHich I probably don't need to do.
#' 
#' See \link{\code{rtLarvalDepositionGrid}} for the grid version.


#' @param vFem a vector of the age distribution of Females 
#' @param vpDeposit a vector of age-specific deposition probabilities of Females 
#' 
#' @return a list containing integer numbers of larvae: iLarvaeF, iLarvaeM
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
  
  #assign gender and round down to integer here
  iLarvaeF <- floor(fLarvae/2)
  iLarvaeM <- floor(fLarvae/2)  
  
  #it would be easy to make the model probabilistic by assigning gender with a 0.5 probability
  
  #return
  invisible( list(iLarvaeF=iLarvaeF, iLarvaeM=iLarvaeM) )
  
} #end of rtLarvalDeposition