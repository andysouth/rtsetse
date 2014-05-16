\name{rtMortality}
\alias{rtMortality}
\title{tsetse mortality}
\usage{
rtMortality(vFem, vMal, vpMortF, vpMortM)
}
\arguments{
  \item{vFem}{a vector of the age distribution of Females}

  \item{vMal}{a vector of the age distribution of Males}

  \item{vpMortF}{a vector of age-specific mortality
  probabilities of Females}

  \item{vpMortM}{a vector of age-specific mortality
  probabilities of Males}
}
\value{
a list containing vFem & vMal
}
\description{
\code{rtMortality} returns a list of the age distributions
of males & females \cr !It should check that the length of
the age structure and pMort vectors are the same \cr It
uses the length of the age structure vectors passed to it.
\cr There could be an option to do F only if only F are
passed.
}

