#' ageing of adults or pupae in all cells on a passed grid
#' 
#' \code{rtAgeingGrid} advances the age of all classes in the vectors in each cell of the passed grid.  
#' note this doesn't call \code{\link{rtAgeing}} which is used in the aspatial model.

#' @param a an array of the age distributions in each grid cell [y,x,sex,age] 
#' @param label a string describing what is being passed, added to any warning messages 
#' 
#' @return an updated array
#' @export

rtAgeingGrid <- function( a, label )
{
  
  #set all numbers in [age] to those from [age-1]
  
  
  #how to do this in a vectorised way
  #can just use c() & stick a zero on the start
  #v <- c(0,v[-length(v)])
  
  #returning updated vector
  #invisible( v )
  
  #############################
  #intermediate test without Cpp
  #this is still slow, removing .drop=FALSE didn't decrease speed
  #37 secs (4 days 100x100)
#   a <- aaply(a, .margins=c(1,2),.drop=FALSE, function(v) c(0,v[-length(v)]) )
  
  #try speed of a loop, this was faster than the above, 9 secs rather than 37
  #9 secs
#   for(x in seq(dim(a)[1])) {
#     for(y in seq(dim(a)[2])) {
#       a[x,y,] <- c(0, a[x,y,-length(a[x,y,])])
#     }
#   }
  
  #now try using apply
  #9 secs
#   a <- apply(a, MARGIN=c(1,2), function(v) c(0,v[-length(v)]) )
#   a <- aperm(a, c(2,3,1))


#previous versions were just for an array of F
#this works on both M&F
  for(y in seq(dim(a)[1])) {
    for(x in seq(dim(a)[2])) {
      #for(mf in seq(dim(a)[3])) {
        a[y,x,'F',] <- c(0, a[y,x,'F',-length(a[y,x,'F',])])
        a[y,x,'M',] <- c(0, a[y,x,'M',-length(a[y,x,'M',])])
        #}
    }
  }

  
#   ############
#   #trying Rcpp
#   library(Rcpp)
#   #later would want to use sourceCpp() to source this from a file
#   cppFunction('NumericVector rowSumsC(NumericMatrix x) {
#     int nrow = x.nrow(), ncol = x.ncol();
#     NumericVector out(nrow);
#   
#     for (int i = 0; i < nrow; i++) {
#       double total = 0;
#       for (int j = 0; j < ncol; j++) {
#         total += x(i, j);
#       }
#       out[i] = total;
#     }
#     return out;
#   }')
#   
#   #x <- matrix(sample(100), 10)
#   x <- matrix(a[1,,])
#   rowSumsC(x)
  
  #returning updated array
  invisible( a )   
} 