#' tsetse pupal emergence
#'
#' \code{rtPupalEmergence} returns the number of emerging pupa
#' 
#' pupal emergence needs to come from the number of pupae in the ground of the appropriate age 
#' 
#' Initially just gets the number of pupae from the final
#' age class of the passed vector.
#' Ideally I would like to set the number in that final age class to 0 at the same time
#' However I can't easily pass 2 objects back ...
#' Would be less code if not in a function ...
#' Want to allow for it being more complex in future ...
#' Can get it to return vectors of both pupae & adults


#' @param vPupaF a vector of pupal Females by age 
#' @param vPupaM a vector of pupal Males by age 
#' @param vPopF a vector of adult Females by age 
#' @param vPopM a vector of adult Males by age 
#' 
#' @return a list containing vPupaF,vPupaM,vPopF & vPopM
#' @export

rtPupalEmergence <- function( vPupaF                                
                            , vPupaM
                            , vPopF
                            , vPopM )
{
  
  #this follows hat-trick in that all pupae in final age class emerge
  #it could be made more probabilistic
  
  #set 1st age class of adults from last age class of pupae
  vPopF[1] <- vPupaF[length(vPupaF)]
  vPopM[1] <- vPupaM[length(vPupaM)]  
  #set last age class of pupae to 0
  vPupaF[length(vPupaF)] <- 0
  vPupaM[length(vPupaM)] <- 0
  
  #return the modified vectors
  invisible( list(vPopF=vPopF,vPopM=vPopM,vPupaF=vPupaF,vPupaM=vPupaM) )
  
}