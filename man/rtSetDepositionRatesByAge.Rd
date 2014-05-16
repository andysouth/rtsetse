\name{rtSetDepositionRatesByAge}
\alias{rtSetDepositionRatesByAge}
\title{setting Age Specific Larval Deposition rates for Tsetse}
\usage{
rtSetDepositionRatesByAge(vPopF, vPopM, iFirstLarva = 16, iInterLarva = 10,
  pMortLarva = 0.05, propMortDD = 0, iCarryCap = NA)
}
\arguments{
  \item{vPopF}{a vector of the age distribution of Females}

  \item{vPopM}{a vector of the age distribution of Males,
  needed to assess density dependence}

  \item{iFirstLarva}{age that first larva deposited}

  \item{iInterLarva}{gap between deposition of larvae}

  \item{pMortLarva}{larval mortality rate}

  \item{propDD}{proportion of mortality that is density
  dependent, set to 0 as default}

  \item{iCarryCap}{Carrying Capacity as an integer, not
  needed if propDD=0}
}
\value{
a vector of larval deposition probabilities
}
\description{
\code{rtSetDepositionRatesByAge}
}
\details{
Gets length of vector from vPopF uses age of first larva,
interlava period & larval mortality rate to create a vector
of deposition rates
}

