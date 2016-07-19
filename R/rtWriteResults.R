#' to write model results to a txt file 
#'
#' \code{rtWriteResults} will write which [days,y,x,sex,age] you want to get data for.  
#' Works by calling \code{rtGetFromRecord()} 

#' @param aRecord an array with the age distributions of males & females [y,x,sex,age]
#' @param file name of file to save results to 
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
#' a small example to start
#' aDays <- rt_runGrid()
#' 
#' # sum age & sex
#' rtWriteResults(aDays, age="sum", sex="sum")
#' 
#' # both sexes, sum age
#' rtWriteResults(aDays, age="sum", sex="all")
#' 
#' # both sexes, all ages (big file)
#' rtWriteResults(aDays, age="all", sex="all") 
#' 
#' # user defined age-range
#' rtWriteResults(aDays, age=c(20:120), sex="all") 
#' 
#' # map for comparison
#' rtPlotMapPop(aDays)
#' 


#' @export


rtWriteResults <- function( aRecord,
                            file="outresults.txt",
                             days='all',
                             x='all',
                             y='all',
                             sex='MF',
                             age='sum',
                             ageSum='sum',
                             drop=TRUE,
                             verbose=FALSE
) 
{
  
  results <- rtGetFromRecord( aRecord, days=days, y=y, x=x, sex=sex, age=age, drop=drop )
  
  #todo may want to check that file is a suitable location, do I want a default ?
  
  #todo, need to format the data in some way for csv writing
  #write.csv(results, file=file)
  
  #rearranging dimensions for output of grids
  #needs to be y,x,z
  # if (length(dim(results))==3)
  # {
  #   #for new 'M&F' option dims already in correct order
  #   if (names(dimnames(results)[1])!='y')
  #     results <- aperm(results, c(2, 3, 1))   
  # }  
  
  posx <- which("x" == names(dimnames(results)))
  posy <- which("y" == names(dimnames(results)))
  posday <- which("day" == names(dimnames(results)))
  
  #get yx to be first dimensions
  #day to be last if it is in, other dimensions in between
  
  if (length(posx)>0 & length(posy)>0 & length(posday)>0)
  {
    results <- aperm(results, c("y", "x", names(dimnames(results))[c(-posy,-posx,-posday)], "day")) 
    
  }  else if (length(posx)>0 & length(posy)>0)
  {
    results <- aperm(results, c("y", "x", names(dimnames(results))[c(-posy,-posx)])) 
  }
  
  
  
  # sink() redirects console output to a file
  sink(file=file)
  cat("rtsetse results created : ")
  cat(date(), "\n")
  # add the command & args used to produce the file
  # doesn't work well because it adds the whole input array
  #lNamedArgs <- mget(names(formals()),sys.frame(sys.nframe()))
  #cat("from rtWriteResults with arguments:",paste0(names(lNamedArgs),"=",lNamedArgs,","),"\n")
  
  print(results)
  
  sink() # reset output to console
  
  
  # return the results structure to facilitate checking
  #invisible(results)
  return(results)  
}