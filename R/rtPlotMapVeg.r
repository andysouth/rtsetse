#' plots a vegetation map (raster)
#' 
#' \code{rtPlotMapVeg} plots vegetation map 
#' either from a file or a matrix object
#' initially accepts hat-trick format vegetation maps
#' which have character values of : D T O S B G N for 7 categories

#' @param map either a file or an object
#' @param title a title for the plot  
#' @param col colours to use for the vegetation categories, default value gives similar to hat-trick
#' @param veg text labels for vegetation categories, default value gives similar to hat-trick
#' 
#' @return a raster object (numeric) of the passed map
# @examples
#' @export

rtPlotMapVeg <- function( map,
                          title = NULL,
                          col = c("white", "dark green", "forest green", "green1", "light blue", "orange", "pale goldenrod"), 
                          veg = c("white", "dark green", "forest green", "green1", "light blue", "orange", "pale goldenrod")
                          )
{

  #if a character string is passed try to open a file
  if (class(map)=='character')
  {
    #read into a dataframe
    map <- read.table(map, as.is=TRUE)
    #convert to a character matrix
    mapMatrix <- as.matrix(map)
  } else if (class(map)=='matrix')
  {
    mapMatrix <- map
  } else
  {
    stop("map is expected to be a filename or a character matrix, yours is:",class(map))
  }

                            
  #all these steps do seem to be necessary to convert to a numeric matrix then raster

  
  
  #replacing characters with numbers
  mapMatrix <- gsub("D",1,mapMatrix)
  mapMatrix <- gsub("T",2,mapMatrix)
  mapMatrix <- gsub("O",3,mapMatrix)
  mapMatrix <- gsub("S",4,mapMatrix)
  mapMatrix <- gsub("B",5,mapMatrix)
  mapMatrix <- gsub("G",6,mapMatrix)
  mapMatrix <- gsub("N",0,mapMatrix)
  
  mapNumeric <- as.numeric(mapMatrix)
  #mapFactor <- as.factor(mapMatrix)
  #mapNumeric <- as.numeric(mapFactor)
  
  mapMatrixNumeric <- matrix(mapNumeric,nrow=nrow(mapMatrix))
  #convert to raster object
  mapRaster <- raster(mapMatrixNumeric)
  
  #plot
  #this nearly works, but breaks screws up plotting and I'm missing one colour
  plot(mapRaster, col=col, main=title, breaks=c(0.5:6.5), lab.breaks=veg)
  
  #plot(mapRaster, col=col, main=title)
  
  #legend=FALSE turns off the default legend
  #
  
  #returning raster object
  invisible(mapRaster)
}