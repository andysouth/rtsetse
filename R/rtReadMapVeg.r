#' reads a vegetation map (raster) from a text or gridascii file
#' 
#' \code{rtReadMapVeg} reads vegetation map 
#' either from a text or gridascii file
#' initially accepts hat-trick format vegetation maps
#' which have character values of : D T O S B G N for 7 categories

#' @param inFile filepath
#' 
#' @return a matrix (character) of the passed map
#' TODO it might be better to return a raster, then I can use it to store info from the gridascii header
# @examples
#' @export

rtReadMapVeg <- function( inFile ) {
  
  #converting to a matrix and modifying dimensions so columns aren't labelled V1 etc
  
  #TODO add checks for what the file can be
  
  #use this to test if it has a gridAscii header
  if (scan(inFile,"character",nmax=1,quiet=TRUE)=="ncols")
  {
    header <- TRUE
    skip <- 6 #BEWARE that file has to have full gridascii header
    #can read the cellsize here
    
  } else 
  {
    skip <- 0    
  }
  
  #read file, skipping the header if specified above
  mat <- as.matrix( read.table(inFile, skip=skip, as.is=TRUE) )
  #sort dimnames that appear in file table
  #reverse y so that 1 is at lower left
  #I could use this to make the lables correspond to latlons in future
  #dimnames(mat)[[1]] <- c(nrow(mat):1)
  #dimnames(mat)[[2]] <- c(1:ncol(mat)) 
  #add x& y for further clarity
  dimnames(mat)[[1]] <- c(paste0("y",nrow(mat):1))
  dimnames(mat)[[2]] <- c(paste0("x",1:ncol(mat))) 
  #these names are then transposed below
  names(dimnames(mat)) <- c("y","x")
  
  #BEWARE! matrix dimensioning trickiness
  #transpose so that the dimensions are x,y
  mat <- t(mat)
  
  
  #returning matrix
  return(mat)
}