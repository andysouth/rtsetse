\name{rtPupalMortality}
\alias{rtPupalMortality}
\title{tsetse pupal mortality}
\usage{
rtPupalMortality(vPupaF, vPupaM, pMort = 0.25, propDD = 0.25,
  iCarryCapPupa = NA)
}
\arguments{
  \item{vPupaF}{a vector of female pupae by age}

  \item{vPupaM}{a vector of male pupae by age}

  \item{pMort}{a mortality probability}

  \item{propDD}{proportion of mortality that is density
  dependent}

  \item{iCarryCapPupa}{Carrying Capacity as an integer}
}
\value{
a list of vectors of pupae by age (vPupaF & vPupaM)
}
\description{
\code{rtPupalMortality} applies pupal mortality once in the
pupal period hat-trick applied same rate for males and
females !I may need to pass pupal carrying capacity and
vegetation type to this initially this was called for each
sex. but because density dependence needs to know about
both sexes it may make more sense to do together
}

