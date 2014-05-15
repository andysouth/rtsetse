\name{rtSetDepositionRatesByAge}
\alias{rtSetDepositionRatesByAge}
\title{setting Age Specific Larval Deposition rates for Tsetse}
\usage{
rtSetDepositionRatesByAge(vFem, iFirstLarva = 16, iInterLarva = 10,
  pMortLarva = 0.05)
}
\arguments{
  \item{vFem}{a vector of the age distribution of Females}

  \item{iFirstLarva}{age that first larva deposited}

  \item{iInterLarva}{gap between deposition of larvae}

  \item{pMortLarva}{larval mortality rate}
}
\value{
a vector of larval deposition probabilities
}
\description{
\code{rtSetDepositionRatesByAge}
}
\details{
Gets length of vector from vFem uses age of first larva,
interlava period & larval mortality rate to create a vector
of deposition rates
}

