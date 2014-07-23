#' to access population data from a record array [day,x,y,sex,age]
#'
#' \code{rtGetFromRecord} allows access to population data from a grid of sexes and ages. 
#' You can specify which [day,x,y,sex,age] you want to get data for.  
#' Each variable defaults to 'all' so \code{rtGetFromRecord(aRecord)} would return the whole grid.   
#' ='sum' can be used to sum across dimensions, 
#' thus \code{rtGetFromRecord(aRecord,x='sum',y='sum',sex='sum',age='sum')} 
#' would produce a single value of the total population on the grid.

#' @param aRecord an array with the age distributions of males & females [x,y,sex,age] 
#' @param day day of the simulation starting at 1
#' @param x grid column number 
#' @param y grid row number 
#' @param sex 'all' returns both sexes separately,'M','F', 'sum' sums sexes
#' @param age 'all' returns age distribution, 'sum' sums all ages, or an integer age
#'     or an age range too, e.g. c(2:3)
#' 
#' @return an array, matrix or vector named with remaining dimensions of [x,y,sex,age]
#' @examples
#' aRecord <- rtPhase2Test2()
#' rtGetFromRecord(aRecord,day=2,x='sum',y='sum',sex='sum') #age structure for whole pop
#' rtGetFromRecord(aRecord,day=2,x='sum',y='sum',age='sum') #sex ratio for whole pop
#' #slight anomally this gives 4 grids
#' rtGetFromRecord(aRecord,day=2,x='all',y='all',age=c(1,2),sex='all')
#' #this gives just 1
#' rtGetFromRecord(aRecord,day=2,x='all',y='all',age=c(1,2),sex='sum')
#' @export


rtGetFromRecord <- function( aRecord,
                           day='all',
                           x='all',
                           y='all',
                           sex='all',
                           age='all'
                           ) 
{
  #?? I may want the default to be sum rather than all
  
  #aRecord[x,y,sex,age]
  allArgs <- c(day,x,y,sex,age)
  
  if ( class(aRecord)!="array" | length(dim(aRecord)) != 5 )
       stop("the first arg needs to be an array with 5 dimensions [day,x,y,sex,age], yours is a ",class(aRecord)," with length(dim)=",length(dim(aRecord)),"\n")
  #I may also want to test that 
  #if (identical( names(dimnames(aRecord)),c("day","x","y","sex","age")))
  
  #add tests that x,y,sex & age have appropriate values
  
  #see table in liverpoolNotes from 23/7/14 about how to access different parts of array
  
  #convert 'all' values to a variable that can be used to
  #access all elements of an array
  #allArgs[ which(allArgs=='all') ] <- TRUE
  if ( identical(day,'all')) day<-TRUE
  if ( identical(x,'all')) x<-TRUE
  if ( identical(y,'all')) y<-TRUE
  if ( identical(age,'all')) age<-TRUE
  if ( identical(sex,'all')) sex<-TRUE
  
  #if no 'sum' arguments
  if (! 'sum' %in% allArgs )
  {
    #this allows for e.g. age=c(1:10)
    toReturn <- aRecord[day,x,y,sex,age, drop=FALSE]
    
  } else if (! 'all' %in% allArgs )
  # if at least one 'sum' but no 'all'  
  { 
    #now change the sum args to TRUE so that they are summed in the sum statement
    if ( identical(day,'sum')) day<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE    
    
    toReturn <- sum( aRecord[day,x,y,sex,age] ) 
    
  } else
  # if at least 1 'sum' & 'all'  
  {    
    #the 'all' args need to be added to the MARGIN arg of apply
    marginArg <- NULL
#     if ( identical(x,'all')) marginArg <- c(marginArg,'x')
#     if ( identical(y,'all')) marginArg <- c(marginArg,'y')
#     if ( identical(age,'all')) marginArg <- c(marginArg,'age')
#     if ( identical(sex,'all')) marginArg <- c(marginArg,'sex') 
    #the alls are converted to TRUE above
    #but can't do if(x) in case x=='sum
    #adding | if( length(x)>1 ) would allow user to get back multiple cells rather than summing
    #but this way is probably more useful
    if ( identical(day,TRUE)) marginArg <- c(marginArg,'day')
    if ( identical(x,TRUE)) marginArg <- c(marginArg,'x')
    if ( identical(y,TRUE)) marginArg <- c(marginArg,'y')
    if ( identical(age,TRUE)) marginArg <- c(marginArg,'age')
    if ( identical(sex,TRUE)) marginArg <- c(marginArg,'sex')  

    #now change any sum args to TRUE so that all vals are included in the apply statement
    if ( identical(day,'sum')) day<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE   
    
    #toReturn <- apply( aRecord, MARGIN=marginArg, sum )

    toReturn <- apply( aRecord[day,x,y,sex,age], MARGIN=marginArg, sum )
  }
    
  
  return(toReturn)
}