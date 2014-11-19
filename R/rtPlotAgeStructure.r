#' plots age structure over multiple days
#' 
#' \code{rtPlotAgeStructure} visualises change in age structure over time using a raster type plot.
#' \cr Requires the raster package.

#' @param df a dataframe with days in columns and ages in rows
#'     OR an array of [day,y,x,sex,age]
#' @param title a title for the plot  
#' 
#' @return nothing
#' @examples
#' tst <- rtPhase1Test2()
#' rtPlotAgeStructure(tst$dfRecordF) 
#' @export

rtPlotAgeStructure <- function( df, title=NULL )
{

  #! 17/7/14 this is a temporary hack to accept
  #a different output from rtPhase2Test2
  #an array of [day,x,y,sex,age]
  #I should make it accept this properly and give options to do
  #'M','F','MF' and even selected grid cells later
  if (class(df)=='array') {
    
    #!!NOTE this sums for both M&F
    
    mAgesByDay <- apply(df,MARGIN=c('day','age'),sum) #summed age structure across grid for each day
    #change day & age dimensions
    mAgesByDay <- aperm(mAgesByDay, c(2,1))
    #convert the matrix to a raster object
    rast <- raster::raster(mAgesByDay)
    #flip the y dimension to get ages starting at bottom
    rast <- raster::flip(rast, direction='y')
    
  } else {
    #this is the original code accepting the output from rtPhase1Test2
    #convert the dataframe to a matrix object
    mat <- as.matrix(df)
    #convert the matrix to a raster object
    rast <- raster::raster(mat)
  }
  

  
  #plot(rast)
  #plot(rast,xlab='days',ylab='ages',axes=FALSE)
  #trying to add sensible axis for days
  #text(x=axTicks(side=1),y=0,cex=0.7,labels=days*axTicks(side=1))
  #can also add them at the top
  #text(x=axTicks(side=1),y=1,cex=0.7,labels=days*axTicks(side=1))
  
  #AHA! changing the extents did it
  #this just assumes ages & days are equal to the number of cells
  raster::extent(rast) <- raster::extent(c(0, ncol(rast), 0, nrow(rast)))
  #findMethods("plot", "package:raster")
  plot(rast,xlab='days',ylab='ages',main=title)
  
  #previous failed effort to set axis labels
  #shows that labels outside of the main plot area still get cut out
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