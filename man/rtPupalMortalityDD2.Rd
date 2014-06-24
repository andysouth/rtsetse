\name{rtPupalMortalityDD2}
\alias{rtPupalMortalityDD2}
\title{tsetse pupal mortality using alternative density dependence mechanism}
\usage{
rtPupalMortalityDD2(vPupaF, vPupaM, pMort = 0.25, iCarryCapPupa = NA,
  fPopNow = NA, fPopPre = NA)
}
\arguments{
  \item{vPupaF}{a vector of female pupae by age}

  \item{vPupaM}{a vector of male pupae by age}

  \item{pMort}{a mortality probability}

  \item{iCarryCapPupa}{Carrying Capacity as an integer}

  \item{fPopNow}{popn today}

  \item{fPopPre}{popn day before}
}
\value{
a list of vectors of pupae by age (vPupaF & vPupaM)
}
\description{
\code{rtPupalMortalityDD2} applies pupal mortality once in
the pupal period. \cr Same rate for males and females (as
in Hat-Trick). \cr Initially this was called for each sex,
but because density dependence needs to know about both
sexes I changed so that both are done in same function
call. \cr !Later I may need to pass vegetation type to this
or another function that modifies vital rates before
passing.
}

