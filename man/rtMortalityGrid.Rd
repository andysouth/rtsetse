\name{rtMortalityGrid}
\alias{rtMortalityGrid}
\title{tsetse mortality on a grid **in development**}
\usage{
rtMortalityGrid(aGrid, vpMortF, vpMortM, propDD = 0.25, mCarryCap = NA,
  loop = TRUE)
}
\arguments{
  \item{aGrid}{an array with the age distributions of males
  & females [x,y,sex,age]}

  \item{vpMortF}{a vector of age-specific mortality
  probabilities of Females}

  \item{vpMortM}{a vector of age-specific mortality
  probabilities of Males}

  \item{propDD}{proportion of mortality that is density
  dependent}

  \item{mCarryCap}{a matrix of Carrying Capacities for each
  cell as an integer}
}
\value{
an array with the age distributions of males & females
[x,y,sex,age]
}
\description{
\code{rtMortalityGrid} returns an array with the age
distributions of males & females [x,y,sex,age] \cr It
accepts age and sex specific mortality probabilities. \cr
!It should check that the length of the age structure and
pMort vectors are the same \cr It uses the length of the
age structure vectors passed to it. \cr There could be an
option to do F only if only F are passed.
}

