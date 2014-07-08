\name{rtLarvalDepositionGrid}
\alias{rtLarvalDepositionGrid}
\title{tsetse larval deposition on a grid}
\usage{
rtLarvalDepositionGrid(aGrid, aGridPup, vpDeposit)
}
\arguments{
  \item{aGrid}{array with the age distributions of males &
  females [x,y,sex,age]}

  \item{aGridPup}{array with the age distributions of pupal
  males & females [x,y,sex,age]}

  \item{vpDeposit}{a vector of age-specific deposition
  probabilities of Females}
}
\value{
a list containing integer numbers of larvae: iLarvaeF,
iLarvaeM
}
\description{
\code{rtLarvalDepositionGrid} returns the number of male
and female larvae deposited in each grid cell
}
\details{
from the age structure of adult females and 'birth'
probabilities per female.
}

