\name{rtPlotMapPop}
\alias{rtPlotMapPop}
\title{plots map(s) of populations}
\usage{
rtPlotMapPop(aRecord, days = "all", ifManyDays = "last", sex = "MF",
  title = NULL)
}
\arguments{
  \item{aRecord}{array of day,x,y,sex,age}

  \item{days}{which days to plot, options 'all', 'final', a
  number >0 and <days in the simulation, or a series e.g.
  c(1,2) or c(4:7)}

  \item{ifManyDays}{days to plot if days='all' and num days
  > 16, Options: 'first' first 16 days, 'last' 16 days,
  'firstlast' first8 and last8, 'spread' try to spread out
  days, e.g. if 32 it would plot 2,4,6, etc. can lead to
  uneven intervals.}

  \item{sex}{which sex to plot, 'both' or 'MF' for both,
  'M' males, 'F' females}

  \item{title}{a title for the plot}
}
\value{
?nothing
}
\description{
\code{rtPlotMapPop} plots map(s) of populations from
simulation outputs i.e. a passed array of day,x,y,sex,age.
Can plot a maximum of 16 days, will display the first 16 if
you select more than this. For adults to start with but
could be used for pupae too.
}
\examples{
tst <- rtPhase2Test2()
rtPlotMapPop(tst)
}

