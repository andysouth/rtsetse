#' plots age structure over multiple days
#' 
#' \code{rtPlotAgeStructure} visualises change in age structure over time using a raster type plot.
#' \cr Requires the raster package.

#' @param df a dataframe with days in columns and ages in rows
#' @param title a title for the plot  
#' 
#' @return nothing
#' @export

rtPlotAgeStructure <- function( df, title=NULL )
{
  #library(raster)
  
  #convert the dataframe to a matrix object
  mat <- as.matrix(df)
  #convert the matrix to a raster object
  rast <- raster(mat)
  
  #plot(rast)
  #plot(rast,xlab='days',ylab='ages',axes=FALSE)
  #trying to add sensible axis for days
  #text(x=axTicks(side=1),y=0,cex=0.7,labels=days*axTicks(side=1))
  #can also add them at the top
  #text(x=axTicks(side=1),y=1,cex=0.7,labels=days*axTicks(side=1))
  
  #AHA! changing the extents did it
  #this just assumes ages & days are equal to the number of cells
  extent(rast) <- extent(c(0, ncol(rast), 0, nrow(rast)))
  plot(rast,xlab='days',ylab='ages',main=title)
  
  #I would still really like to be able to set the axes outside of the plot region itself
  #maybe fun in plot rast can help ?
  #No this shows that labels outside of the main plot area still get cut out
  #fun <- function() {
  #  axis(side=1, at=axTicks(side=1), labels=days*axTicks(side=1),line=-1.5 )
  #}
  #plot(rast, addfun=fun)
  
  
  #aha this starts working but for some reason -5 is invisible ????
  #axis(side=1, at=axTicks(side=1), labels=days*axTicks(side=1),line=-6 )
  #axis(side=1, at=axTicks(side=1), labels=days*axTicks(side=1),line=-2 )
  #this created more tick marks but didn't allow me to alter labels
  #plot(rast,xlab='days',ylab='ages',xaxp=c(0,1,10))  

  
} 