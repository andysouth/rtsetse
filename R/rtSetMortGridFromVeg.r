#' setting a grid of mortality multipliers from a vegetation grid
#'
#' \code{rtSetMortGridFromVeg} 
#' creates a grid of multipliers to be applied to the standard adult or pupa mortality rates.
#' From a vegetation grid and a table specifying the multiplier for each vegetation.
#' if the passed matrix contains a code not present in the table it returns NA.


#' @param sAdultOrPupa "adult" or "pupa"
#' @param mVegetation a matrix of vegetation types
#' @param dfMortByVeg a lookup table specifying mortality multiplier (percent) for each vegetation type
#' 
#' @return a grid of mortality multipliers
#' 
#' @examples
#' mMortMult <- rtSetMortGridFromVeg("adult")
#' @export

rtSetMortGridFromVeg <- function( sAdultOrPupa = "",
                                  mVegetation = array(c("D","T","O","S","N","N"),dim=c(2,3)),
                                  dfMortByVeg = data.frame(code=c("D","T","O","S","B","G","N"),mortality=c(200,150,110,100,110,210,999),pupmortality=c(120,110,105,100,120,170,999),stringsAsFactors = FALSE)
                                  )
{
  
  sAdultOrPupaColumn <- ""
  if ( sAdultOrPupa == "adult") sAdultOrPupaColumn <- "mortality"
  else if ( sAdultOrPupa == "pupa") sAdultOrPupaColumn <- "pupmortality"  
  else if ( sAdultOrPupa == "") stop("you need to specify adult or pupa")
  else stop("you need to specify 'adult' or 'pupa' you have",sAdultOrPupa)
  
  #gives a warning if the matrix gives a code not present in the df
  #currently it returns NA, may want to modify that
  #if ( NA %in% match(mVegetation, dfMortByVeg$code) )
  #badCodePosns <- which(is.na( match(mVegetation, dfMortByVeg$code))) 
  badCodePosns <- which(!mVegetation %in% dfMortByVeg$code)
  badCodes <- unique( mVegetation[ badCodePosns ])
  
  #match returns vector of positions of (first) matches of arg1 in arg2
  #vMortMult <- dfMortByVeg$mortality[ match(mVegetation, dfMortByVeg$code)]
  #now allowing for ad or pup column
  vMortMult <- dfMortByVeg[[sAdultOrPupaColumn]][ match(mVegetation, dfMortByVeg$code)]
  
  if ( length(badCodePosns) > 0 )
  {
    warning("some codes in your grid don't match those in the lookup: ",paste(badCodes,""),"\n",
            "mortality for these codes will be set to 999 * standard\n" )
    #TODO what should I do with bad codes ?
    #I could set them to the standard mortality, 
    #or to 999 * mortality to make it clear there is a problem with them
    #could do that by adding the bad codes on to the lookup
    #this is easier
    vMortMult[ badCodePosns ] <- 999
  }
  
  #resetting the matrix dimensions from the passed matrix prior to returning
  mMortMult <- matrix(vMortMult, nrow=nrow(mVegetation))
  
  invisible(mMortMult)
}