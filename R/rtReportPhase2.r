#' produces a report for one run NOTE! this is not used by shiny apps they have their own similar code.
#'
#' \code{rtReportPhase2} creates a pdf of text and graphical output for this run.  

#' @param aRecord output from phase2 a multi-dimensional array for adults [day,y,x,sex,ages] *later it may need to include pupae*
#' @param lNamedArgs a list of the arguments and their values passed to rtPhase2Test2
#' @param filename a name for the report file
#' 
#' @return ?nothing maybe
#' @examples
#' #tst <- rt_runGridTestSpread()
#' #rtReportPhase2(tst, lNamedArgs, filename="myoutput.pdf")
#' @export
#' 
rtReportPhase2 <- function( aRecord,
                            lNamedArgs,
                            filename = "reportPhase2.html" ) 
{
  
#   #this whole option had poor formatting
#   pdf(filename)
#   #trying out gplots::textplot
#   library(gplots)
#   gplots::textplot("First go at creating rtsetse report\n")
#   rtPlotPopGrid( aRecord, title="Population over the whole grid")
#   dev.off()
  
  ###############################
  #trying using rmarkdown instead
  #library(rmarkdown)
  #rmarkdown::render('rtReportPhase2.Rmd',"pdf_document")
  #rmarkdown::render('rtReportPhase2.Rmd', envir=.GlobalEnv)
  #rmarkdown::render('rtReportPhase2.Rmd')
  #rmarkdown::render('inst//rmarkdown//rtReportPhase2.Rmd')
  #looks for the Rmd file in inst/rmarkdown
  filePath <- system.file("rmarkdown", "rtReportPhase2.Rmd", package = "rtsetse")
  rmarkdown::render(filePath, output_file=filename)
  
}