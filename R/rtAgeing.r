#' ageing of adults or pupae
#' 
#' \code{rtAgeing} simply advances the age of all classes in the passed vector

#' @param v a vector of age distribution 
#' @param label a string describing what is being passed, added to any warning messages 
#' 
#' @return an updated vector
#' @export

rtAgeing <- function( v, label )
{
  
  #set all numbers in [age] to those from [age-1]
  #!if there are any in the max age class they will be lost here
  #!add a warning to that effect
  if ( v[length(v)] != 0 )
    message("in rtAgeing members of final age class will be lost :",v[length(v)]," ",label)
    #warning()
  
  #go down through ages to avoid double counting
  for( age in length(v):2 )  {
    v[age] <- v[age-1]    
  }

  #set age 1 to 0 to avoid potential bugs
  v[1] <- 0
  
  #returning updated vector
  invisible( v )
  
} 