#' to access population data from the grid of [x,y,sex,age]
#'
#' NOT USED YET. I might not even need it. rtGetFromRecord() likely to be more useful.
#' \code{rtGetFromGrid} allows access to population data from a grid of sexes and ages. 
#' You can specify which [x,y,sex,age] you want to get data for.  
#' Each variable defaults to 'all' so \code{rtGetFromGrid(aGrid)} would return the whole grid.   
#' ='sum' can be used to sum across dimensions, 
#' thus \code{rtGetFromGrid(aGrid,x='sum',y='sum',sex='sum',age='sum')} 
#' would produce a single value of the total population on the grid.

#' @param aGrid an array with the age distributions of males & females [y,x,sex,age] 
#' @param x grid column number 
#' @param y grid row number 
#' @param sex 'all' returns both sexes separately,'M','F', 'sum' sums sexes
#' @param age 'all' returns age distribution, 'sum' sums all ages, or an integer age
#'     ?or maybe an age range too
#' 
#' @return an array, matrix or vector named with remaining dimensions of [y,x,sex,age]
#' @examples
#' tst <- rtPhase2Test2()
#' aGrid <- tst['day2',,,,]
#' rtGetFromGrid(aGrid,x='sum',y='sum',sex='sum') #age structure for whole pop
#' rtGetFromGrid(aGrid,x='sum',y='sum',age='sum') #sex ratio for whole pop
#' #slight anomally this gives 4 grids
#' rtGetFromGrid(aGrid,x='all',y='all',age=c(1,2),sex='all')
#' #this gives just 1
#' rtGetFromGrid(aGrid,x='all',y='all',age=c(1,2),sex='sum')
#' @export


rtGetFromGrid <- function( aGrid,
                           y='all',
                           x='all',
                           sex='all',
                           age='all'
                           ) 
{
  #?? I may want the default to be sum rather than all
  
  #aGrid[x,y,sex,age]
  allArgs <- c(y,x,sex,age)
  
  if ( class(aGrid)!="array" | length(dim(aGrid)) != 4 )
       stop("the first arg needs to be an array with 4 dimensions [y,x,sex,age], yours is a ",class(aGrid)," with length(dim)=",length(dim(aGrid)),"\n")
  #I may also want to test that 
  #if (identical( names(dimnames(aGrid)),c("day","y","x","sex","age")))
  
  #add tests that x,y,sex & age have appropriate values
  
  #see table in liverpoolNotes from 23/7/14 about how to access different parts of array
  
  #convert 'all' values to a variable that can be used to
  #access all elements of an array
  #allArgs[ which(allArgs=='all') ] <- TRUE
  if ( identical(y,'all')) y<-TRUE
  if ( identical(x,'all')) x<-TRUE
  if ( identical(age,'all')) age<-TRUE
  if ( identical(sex,'all')) sex<-TRUE
  
  #if no 'sum' arguments
  if (! 'sum' %in% allArgs )
  {
    #this allows for e.g. age=c(1:10)
    toReturn <- aGrid[y,x,sex,age, drop=FALSE]
    
  } else if (! 'all' %in% allArgs )
  # if at least one 'sum' but no 'all'  
  { 
    #now change the sum args to TRUE so that they are summed in the sum statement
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE    
    
    toReturn <- sum( aGrid[y,x,sex,age] ) 
    
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
    if ( identical(y,TRUE)) marginArg <- c(marginArg,'y')
    if ( identical(x,TRUE)) marginArg <- c(marginArg,'x')
    if ( identical(age,TRUE)) marginArg <- c(marginArg,'age')
    if ( identical(sex,TRUE)) marginArg <- c(marginArg,'sex')  

    #now change any sum args to TRUE so that all vals are included in the apply statement
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE   
    
    #toReturn <- apply( aGrid, MARGIN=marginArg, sum )

    toReturn <- apply( aGrid[y,x,sex,age], MARGIN=marginArg, sum )
  }
    
  
  return(toReturn)
}