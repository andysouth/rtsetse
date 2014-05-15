\name{rtDDTest}
\alias{rtDDTest}
\title{tests density dependence and outputs a plot}
\usage{
rtDDTest(vDensities = c(0:20), iCC = 10, propDD = 0.25, pMort = 0.2)
}
\arguments{
  \item{vDensities}{a vector of densities to test}

  \item{iCC}{Carrying Capacity as an integer}

  \item{propDD}{proportion of mortality that is density
  dependent}

  \item{pMort}{a mortality probability}
}
\value{
a dataframe containing densities, relative densities and
resulting mortality rates
}
\description{
\code{rtDDTest} passes a vector of densities to the density
dependence function along with a carrying capacity and
outputs a plot of how the mortality rate changes by
relative density. The default parameeters show an example.
This function is not used in the running of the simulation.
}

