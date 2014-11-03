#' plots a vegetation map (raster)
#' 
#' \code{rtPlotMapVeg} plots vegetation map 
#' either from a file or a matrix object
#' initially accepts hat-trick format vegetation maps
#' which have character values of : D T O S B G N for 7 categories

#' @param map either a file or an object
#' @param title a title for the plot  
#' @param colours to use for the vegetation categories, default value gives similar to hat-trick
#' @param labels text labels for vegetation categories, default value gives similar to hat-trick
#' @param inset for the legend, make it less -ve to move from edge
#' @param pt.cex size of boxes in legend
#' @param cex size of text in legend
#' @param bty whether to have box around legend "o" for yes
#' 
#' @return a raster object (numeric) of the passed map
# @examples
#' @export

rtPlotMapVeg <- function( map,
                          title = NULL,
                          colours = c("dark green", "forest green", "green1", "light blue", "orange", "pale goldenrod", "white" ), 
                          labels = c("dense forest", "thicket", "open forest", "savannah", "bush", "grass", "no go area" ),
                          inset = -0.26,
                          pt.cex = 1.7,
                          cex = 0.9,
                          bty = "n"
                          
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
  mapMatrix <- gsub("N",7,mapMatrix) #0 or 7
  
  mapNumeric <- as.numeric(mapMatrix)
  #mapFactor <- as.factor(mapMatrix)
  #mapNumeric <- as.numeric(mapFactor)
  
  mapMatrixNumeric <- matrix(mapNumeric,nrow=nrow(mapMatrix))
  #convert to raster object
  mapRaster <- raster(mapMatrixNumeric)
  
  #set extents for plotting (otherwise they go from 0-1)
  #this also ensures that cells maintain square aspect ratio 
  extent(mapRaster) <- extent(c(0, ncol(mapRaster), 0, nrow(mapRaster)))
  
  #plot
  #this nearly works, but breaks screws up plotting and I'm missing one colour
  #plot(mapRaster, col=col, main=title, breaks=c(0.5:6.5), lab.breaks=veg)
  
  #plot(mapRaster, col=col, main=title)
  
  #legend=FALSE turns off the default legend
  #
  
  #problem with plot() is that axes expand to fill available space
  #plot(mapRaster,legend=FALSE, col=colours, main=title)
  #legend("right", legend=labels, pch=22, xpd=NA, inset=inset, col="black", pt.bg=colours, pt.cex=pt.cex, cex=cex, bty=bty)
  
  
  #seeing if I can apply a RAT (raster attribute table) to allow me to use levelplot from rasterVis
  #and solve expanding axis problems
#   mapRaster <- ratify(mapRaster) 
#   rat <- levels(mapRaster)[[1]]
#   #rat$legend <- c('Class A', 'Class B', 'Class C', 'Class D')
#   rat$legend <- labels
#   levels(mapRaster) <- rat
#   
#   levelplot(mapRaster)
  #levelplot(mapRaster, fill=colours)
  
  #spplot solution
  #plot( spplot(mapRaster, at=c(-0.5:6.5), col.regions=colours ) )
  #names.attr=labels does nothing
  #zcol=labels gives error

  #when I had no-go set to 0 I used the top one
  #plot( spplot(mapRaster, at=c(-0.5:6.5), col.regions=colours, colorkey=FALSE,
  plot( spplot(mapRaster, at=c(0.5:7.5), col.regions=colours, colorkey=FALSE,
               key= list(space="right",
                        text=list(labels=labels, cex=cex), 
                        points=list(pch=22, fill=colours, cex=pt.cex),
                        between=0.5
                        )) )


#   #can I add my nicer labelled cat legend ?
#   p1 <- spplot(mapRaster, at=c(-0.5:6.5), col.regions=colours, colorkey=FALSE ) 
# 
#   #adding legend using lattice
#   p1 <- update( key=list(text=list(labels), 
#                          points=list(pch=21, 
#                          fill=colours,
#                          space="right")))
#   plot( p1 )

  #returning raster object
  invisible(mapRaster)
}