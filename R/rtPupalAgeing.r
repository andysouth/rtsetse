#' tsetse pupal ageing
#' DEPRECATED probably already replaced with rtAgeing
#' \code{rtPupalAgeing} simply advances the age of all pupa by 1 day
#' Pupal Emergence is likely to be dealt with in rtPupalEmergence


#' @param vPupaF a vector of the age distribution of Female pupae 
#' @param vPupaM a vector of the age distribution of Male pupae 

#' 
#' @return a list containing updated vPupaF & vPupaM
#' @export

rtPupalAgeing <- function( vPupaF                                
                         , vPupaM )
{
  
  #set all numbers in [age] to those from [age-1]
  #!if there are any in the max age class they will be lost here
  #!add a warning to that effect
  if ( vPupaF[length(vPupaF)] != 0 | vPupaM[length(vPupaM)] !=0 )
    message("in rtPupalAgeing pupae in final age class will be lost F:",vPupaF[length(vPupaF)],"  M:",vPupaM[length(vPupaM)],"\n")
    #warning()
  
  #go down through ages to avoid double counting
  for( age in length(vPupaF):2 )  {
    vPupaF[age] <- vPupaF[age-1]    
  }
  for( age in length(vPupaM):2 )  {
    vPupaM[age] <- vPupaM[age-1]   
  }
  
  #set age 1 to 0 to avoid potential bugs
  vPupaF[1] <- vPupaM[1] <- 0
  
  #returning updated vectors
  invisible( list(vPupaF=vPupaF, vPupaM=vPupaM) )
  
} 