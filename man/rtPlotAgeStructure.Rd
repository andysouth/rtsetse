\name{rtPlotAgeStructure}
\alias{rtPlotAgeStructure}
\title{plots age structure over multiple days}
\usage{
rtPlotAgeStructure(df, title = NULL)
}
\arguments{
  \item{df}{a dataframe with days in columns and ages in
  rows}

  \item{title}{a title for the plot}
}
\value{
nothing
}
\description{
\code{rtPlotAgeStructure} visualises change in age
structure over time using a raster type plot. \cr Requires
the raster package.
}
\examples{
tst <- rtPhase1Test2()
rtPlotAgeStructure(tst$dfRecordF)
}

