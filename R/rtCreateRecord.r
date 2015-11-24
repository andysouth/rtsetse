#' create a record array to store a daily record for a spatial, sex & age structured popn
#' 
#' \code{rtCreateRecord} creates a named array with dimensions, day,y,x,sex,age. 
#' calls \code{\link{rtCreateGrid}} 
#' The dimensions are named e.g. x1,x2..., y1,y2... to reduce potential for confusion.
#' To fill array with sequential values after can use :
#' aRecord[] <- c(1:length(aRecord))
#' @param nDay number of days
#' @param nY number of y cells
#' @param nX number of x cells
#' @param sexes vector of sex names default=c("F","M")
#' @param nAge number of ages
#' @param fill what value to fill array with
#' 
#' @return a named array with the specified dimensions
#' @examples
#' aRecord <- rtCreateRecord( nDay=2, nY=3, nX=4,nAge=2, fill=0 )
#' @export
rtCreateRecord <- function( nDay = 2,
                            nY = 3,
                            nX = 4,
                            sexes = c("F","M"),
                            nAge = 2,
                            fill = 0 )
{
  
  #create the grid
  aGrid <- rtCreateGrid(nY=nY, nX=nX, sexes=sexes, nAge=nAge)
  
  #if you wanted to fill with sequential values later you could use
  #aGrid[] <- 1:length(aGrid)
  
  #add first day dimension
  aRecord <- abind::abind(aGrid,along=0)
  
  #add days
  for( day in 2:nDay )
  {
    #bind todays grid [y,x,sex,age] onto a record for all days [day,y,x,sex,age]
    aRecord <- abind::abind(aRecord, aGrid, along=1, use.first.dimnames=TRUE) #along=1 binds on first dimension    
  }
  
  #seems these dimension names get lost
  dimnames(aRecord)[[1]] <- paste0('day',1:nDay)  
  names(dimnames(aRecord)) <- c('day','y','x','sex','age')  
  
  invisible(aRecord)
  
}