\name{rtDDRogersTest}
\alias{rtDDRogersTest}
\title{tests Rogers pupal mortality and outputs a plot}
\usage{
rtDDRogersTest(vDensities = c(150:300), pMort = 0.25,
  iPupaDensThresh = 200, fSlopeDD = 1)
}
\arguments{
  \item{vDensities}{a vector of densities to test}

  \item{iCarryCap}{Carrying Capacity as an integer}

  \item{propDD}{proportion of mortality that is density
  dependent}

  \item{pMort}{a mortality probability}
}
\value{
a dataframe containing densities, relative densities and
resulting mortality rates
}
\description{
\code{rtDDRogersTest} passes a vector of densities to the
density dependence function along with a carrying capacity
and outputs a plot of how the mortality rate changes by
relative density. The default parameters show an example.
This function is not used in the running of the simulation.
}

