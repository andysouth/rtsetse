#' setting a grid of anything from a vegetation grid
#'
#' \code{rtSetGridFromVeg} 
#' creates a grid of anything.
#' From a vegetation grid and a lookup table.
#' if the passed matrix contains a code not present in the table it returns NA.


#' @param mVegetation a matrix of vegetation types
#' @param dfLookup a lookup table specifying a value for each vegetation type
#' 
#' @return a grid of converted values
#' 
#' @examples
#' mTest <- rtSetGridFromVeg()
#' @export

rtSetGridFromVeg <- function(     mVegetation = array(c("D","T","O","S","N","N"),dim=c(2,3)),
                                  dfLookup = data.frame(from=c("D","T","O","S","B","G","N"),to=c(200,150,110,100,110,210,999),stringsAsFactors = FALSE)
                                  )
{
  
  #sometimes dfLookup gets passed as a list, convert it to a data.frame for easier indexing
  dfLookup <- data.frame(dfLookup)
  
  #gives a warning if the matrix gives a code not present in the df
  #currently it returns NA, may want to modify that
  badCodePosns <- which(!mVegetation %in% dfLookup[,1])
  badCodes <- unique( mVegetation[ badCodePosns ])
  
  #match returns vector of positions of (first) matches of arg1 in arg2
  vResult <- dfLookup[,2][ match(mVegetation, dfLookup[,1]) ]
  
  if ( length(badCodePosns) > 0 )
  {
    warning("some codes in your grid don't match those in the lookup: ",paste(badCodes,""),"\n",
            "results for these codes will be set to NA\n" )
    #TODO what should I do with bad codes ?
    vResult[ badCodePosns ] <- NA
  }
  
  #resetting the matrix dimensions from the passed matrix prior to returning
  mResult <- matrix(vResult, nrow=nrow(mVegetation))
  
  invisible(mResult)
}