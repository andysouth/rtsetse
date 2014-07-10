\name{rtPlotMapPop}
\alias{rtPlotMapPop}
\title{plots map(s) of populations}
\usage{
rtPlotMapPop(aRecord, days = "all", title = NULL)
}
\arguments{
  \item{aRecord}{array of day,x,y,sex,age}

  \item{days}{which days to plot, options 'all', a number,
  c(1,2), 'final'}

  \item{title}{a title for the plot}
}
\value{
?nothing
}
\description{
\code{rtPlotMapPop} plots maps of populations from
simulation outputs i.e. a passed array of day,x,y,sex,age.
For adults to start with but could be used for pupae too.
}
\examples{
tst <- rtPhase2Test2()
rtPlotMapPop(tst)
}

