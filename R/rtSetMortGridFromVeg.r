#' setting a grid of mortality multipliers from a vegetation grid
#'
#' \code{rtSetMortGridFromVeg} 
#' creates a grid of multipliers to be applied to the standard mortality rates.
#' From a vegetation grid and a table specifying the multiplier for each vegetation.
#' if the passed matrix contains a code not present in the table it returns NA.

#' @param mVegetation a matrix of vegetation types
#' @param dfMortByVeg a lookup table specifying mortality multiplier (percent) for each vegetation type
#' 
#' @return a grid of mortality multipliers
#' 
#' @examples
#' mMortMult <- rtSetMortGridFromVeg()
#' @export

rtSetMortGridFromVeg <- function( mVegetation = matrix(c("D","T","O","S","D","D"),nrow=2),
                                  dfMortByVeg = data.frame(code=c("D","T","O","S"),mortality=c(100,200,300,400),stringsAsFactors = FALSE)
                                  )
{
  
  #gives a warning if the matrix gives a code not present in the df
  #currently it returns NA, may want to modify that
  #if ( NA %in% match(mVegetation, dfMortByVeg$code) )
  #badCodePosns <- which(is.na( match(mVegetation, dfMortByVeg$code))) 
  badCodePosns <- which(!mVegetation %in% dfMortByVeg$code)
  badCodes <- unique( mVegetation[ badCodePosns ])
  
  #match returns vector of positions of (first) matches of arg1 in arg2
  vMortMult <- dfMortByVeg$mortality[ match(mVegetation, dfMortByVeg$code)]
  
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