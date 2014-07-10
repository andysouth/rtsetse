\name{rtReportPhase2}
\alias{rtReportPhase2}
\title{produces a report on outputs of one run of phase2 using selected parameter values}
\usage{
rtReportPhase2(aRecord, lNamedArgs, filename = "phase2Report.pdf")
}
\arguments{
  \item{aRecord}{output from phase2 a multi-dimensional
  array for adults [day,x,y,sex,ages] *later it may need to
  include pupae*}

  \item{lNamedArgs}{a list of the arguments and their
  values passed to rtPhase2Test2}

  \item{filename}{a name for the report file}
}
\value{
?nothing maybe
}
\description{
\code{rtReportPhase2} creates a pdf of text and graphical
output for this run. *later this may need to include all
the arguments that are passed to rtPhase2Test2
}
\examples{
#tst <- rtPhase2Test2()
#rtReportPhase2(tst, filename="myoutput.pdf")
}

