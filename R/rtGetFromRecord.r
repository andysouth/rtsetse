#' to access population data from a record array [days,y,x,sex,age]
#'
#' \code{rtGetFromRecord} allows access to population data from a grid of sexes and ages. 
#' You can specify which [days,y,x,sex,age] you want to get data for.  
#' Each variable defaults to 'all' so \code{rtGetFromRecord(aRecord)} would return the whole grid.   
#' ='sum' can be used to sum across dimensions, 
#' thus \code{rtGetFromRecord(aRecord,x='sum',y='sum',sex='sum',age='sum')} 
#' would produce a single value of the total population on the grid.

#' @param aRecord an array with the age distributions of males & females [y,x,sex,age] 
#' @param days day of the simulation starting at 1 options 'all', 'sum', 
#'    a number >0 and <days in the simulation, or a series e.g. c(1,2) or c(4:7)
#' @param x grid column number 
#' @param y grid row number 
#' @param sex 'all' returns both sexes separately,'M','F', 'sum' sums sexes
#' @param age 'all' returns age distribution, 'sum' sums all ages, or an integer age
#'     or an age range too, e.g. c(2:3)
#' @param ageSum if='sum'(default) sums ages when age=a range, otherwise returns ages separately     
#' @param drop whether to drop dimensions that just have a single value, TRUE as default 
#' @param verbose print what it's doing T/F


#' @return an array, matrix or vector named with remaining dimensions of [y,x,sex,age]
#' @examples
#' aRecord <- rt_runGridTestSpread()
#' aGrid <- rtGetFromRecord(aRecord,days=2) #gives raw array for one day
#' rtGetFromRecord(aRecord,days=2,x='sum',y='sum',sex='sum') #age structure for whole pop
#' rtGetFromRecord(aRecord,days=2,x='sum',y='sum',age='sum') #sex ratio for whole pop
#' #slight anomally this gives 2 grids for M&F
#' rtGetFromRecord(aRecord,days=2,x='all',y='all',age=c(1,2),sex='all')
#' #this gives 4 grids for M&F age 1&2
#' rtGetFromRecord(aRecord,days=2,x='all',y='all',age=c(1,2),sex='all',ageSum='other')
#' #this gives just 1 grid summed across all, currently tricky to sum by sex but not by age
#' rtGetFromRecord(aRecord,days=2,x='all',y='all',age=c(1,2),sex='sum')
#' #new test case
#' aRecord <- rt_runGridTestSpread(3,3,iDays=4)
#' rtGetFromRecord(aRecord,days=1:3,x='all',y='all',age=c(1,2),sex='sum')
#' #to subset days for a specified cell, do this
#' #tst2 <- rtGetFromRecord(aRecord,x=2,y=2,sex='sum',age='sum',days=c(1:3))
#' #plot(tst2,type='l')

#' @export


rtGetFromRecord <- function( aRecord,
                           days='all',
                           x='all',
                           y='all',
                           sex='all',
                           age='all',
                           ageSum='sum',
                           drop=TRUE,
                           verbose=FALSE
                           ) 
{
  
  #!BEWARE this function is tricky
  if(verbose) message("in rtGetFromRecord() days=",days," y=",y," x=",x," sex=",sex," age=",age," drop=",drop,"\n")
  
  #?? I may want the default to be sum rather than all
  
  #checks
  #check that aRecord is an arry with 5 dimensions
  if ( class(aRecord)!="array" | length(dim(aRecord)) != 5 )
    stop("the first arg needs to be an array with 5 dimensions [day,y,x,sex,age], yours is a ",class(aRecord)," with length(dim)=",length(dim(aRecord)),"\n")
  #check that the dimensions are named as expected
  if ( ! identical( names(dimnames(aRecord)),c("day","y","x","sex","age")) )
    stop("array dimensions should be named day,y,x,sex,age yours are: ", names(dimnames(aRecord)) )
  
  #aRecord[y,x,sex,age]
  #17/12/15 added ageSum to allow summing of passed age ranges
  allArgs <- c(days,y,x,sex,age,ageSum)
  
  #add tests that y,x,sex & age have appropriate values
  
  #see table in liverpoolNotes from 23/7/14 about how to access different parts of array
  
  #convert 'all' values to a variable that can be used to
  #access all elements of an array
  #allArgs[ which(allArgs=='all') ] <- TRUE
  if ( identical(days,'all')) days<-TRUE
  if ( identical(y,'all')) y<-TRUE
  if ( identical(x,'all')) x<-TRUE
  if ( identical(age,'all')) age<-TRUE
  if ( identical(sex,'all')) sex<-TRUE
  
  #if no 'sum' arguments
  if (! 'sum' %in% allArgs )
  {
    #this allows for e.g. age=c(1:10)
    #toReturn <- aRecord[days,y,x,sex,age, drop=FALSE]
    #i think I do want to drop dimensions that go to 1
    #e.g. if selecting a single day
    #I could allow user to pass the drop param & set it to default TRUE
    toReturn <- aRecord[days,y,x,sex,age, drop=drop]

#19/9/14 seems next bit not needed & it stopped the returning of selected days for a single cell
#tst2 <- rtGetFromRecord(aRecord,x=2,y=2,sex='sum',age='sum',days=c(1:3))
#10/11/14 seems it is needed for returning totpop for a selected day
#rtGetFromRecord(aRecord,days=2,x='sum',y='sum',sex='sum',age='sum')
#cool now both fixed by reinstating the loop and adding length(days) condition
#  } else if (! 'all' %in% allArgs )
  } else if (! 'all' %in% allArgs & length(days) == 1 )
  # if at least one 'sum' but no 'all' & just one day 
  { 
    #now change the sum args to TRUE so that they are summed in the sum statement
    if ( identical(days,'sum')) days<-TRUE
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE    
    
    toReturn <- sum( aRecord[days,y,x,sex,age] ) 
    
  } else
  # if at least 1 'sum' & 'all'  
  {    
    #the 'all' args need to be added to the MARGIN arg of apply
    #(they were converted to TRUE above)
    marginArg <- NULL

    #adding | if( length(x)>1 ) would allow user to get back multiple cells rather than summing
    #but this way is probably more useful
    #added length(dimnames(aRecord)... bits to protect against calling it when days,x or y=1
    if ( (identical(days,TRUE) | length(days)>1) & length(dimnames(aRecord)$day) > 1 ) marginArg <- c(marginArg,'day')
    #if ( !identical(days,'sum')) marginArg <- c(marginArg,'days')
    if ( identical(y,TRUE) & length(dimnames(aRecord)$y) > 1) marginArg <- c(marginArg,'y')
    if ( identical(x,TRUE) & length(dimnames(aRecord)$x) > 1 ) marginArg <- c(marginArg,'x')
    if ( identical(age,TRUE)) marginArg <- c(marginArg,'age')
    if ( identical(sex,TRUE)) marginArg <- c(marginArg,'sex')  

    #now change any sum args to TRUE so that all vals are included in the apply statement
    if ( identical(days,'sum')) days<-TRUE
    if ( identical(y,'sum')) y<-TRUE
    if ( identical(x,'sum')) x<-TRUE
    if ( identical(age,'sum')) age<-TRUE
    if ( identical(sex,'sum')) sex<-TRUE   
    
    #toReturn <- apply( aRecord, MARGIN=marginArg, sum )

    toReturn <- apply( aRecord[days,y,x,sex,age], MARGIN=marginArg, sum )
  }
    
  
  return(toReturn)
}