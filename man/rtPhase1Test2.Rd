\name{rtPhase1Test2}
\alias{rtPhase1Test2}
\title{an a-spatial tsetse popn simulation, a second test of phase1 with an alternative density dependence mechanism}
\usage{
rtPhase1Test2(iDays = 30, iMaxAge = 120, iCarryCap = 200,
  iStartAdults = 200, iStartAges = 1, iStartPupae = 200, pMortF = 0.05,
  pMortM = 0.05, iMortMinAgeStart = 10, iMortMinAgeStop = 50,
  fMortMinProp = 0.2, fMortOldProp = 0.3, propMortAdultDD = 0,
  pMortPupa = 0.25, iPupaDensThresh = 200, fSlopeDD = 1,
  propMortPupaDD = 0, iPupDurF = 26, iPupDurM = 28, iFirstLarva = 16,
  iInterLarva = 10, pMortLarva = 0.05, propMortLarvaDD = 0,
  plot = FALSE, verbose = TRUE)
}
\arguments{
  \item{iDays}{days to run simulation}

  \item{iMaxAge}{max age of fly allowed in model (will warn
  if flies age past this)}

  \item{iCarryCap}{carrying capacity of adults}

  \item{iStartAdults}{number of adults to start simulation
  with}

  \item{iStartAges}{spread start adults across the first n
  ages classes}

  \item{iStartPupae}{number of pupae to start simulation
  with (they get spread across sex&age)}

  \item{pMortF}{adult female mortality per day}

  \item{pMortM}{adult male mortality per day}

  \item{iMortMinAgeStart}{Age at which min death rates
  start.}

  \item{iMortMinAgeStop}{Age at which min death rates
  stop.}

  \item{fMortMinProp}{What proportion of the maximum death
  rate on day 0 is the minimum death rate.}

  \item{fMortOldProp}{What proportion of the maximum death
  rate on day 0 is the death rate after iDeathMinAgeStop.}

  \item{propMortAdultDD}{DEPRECATED proportion of adult
  mortality that is density dependent}

  \item{pMortPupa}{pupal mortality per period}

  \item{propMortPupaDD}{DEPRECATED proportion of pupal
  mortality that is density dependent}

  \item{iPupaDensThresh}{the threshold pupal density above
  which density dependence acts}

  \item{fSlopeDD}{the slope of density dependence, how
  mortality increases with density}

  \item{iPupDurF}{days it takes pupa(F) to develop}

  \item{iPupDurM}{days it takes pupa(M) to develop}

  \item{iFirstLarva}{Age that female produces first larva}

  \item{iInterLarva}{Inter-larval period}

  \item{pMortLarva}{larval mortality per period}

  \item{propMortLarvaDD}{DEPRECATED proportion of larval
  mortality that is density dependent}

  \item{plot}{whether to plot graphs}

  \item{verbose}{whether to output progress messages to
  console}
}
\value{
a list of lots: \cr dataframes of adult age structure over
time dfRecordF & M \cr dataframes of adult age structure
over time dfRecordPupa F & M
}
\description{
\code{rtPhase1Test2} runs an a-spatial popn simulation as a
test of phase 1 model components. It applies
density-dependent pupal mortality following Rogers(1990).
Some arguments related to density dependence may not be
needed and are marked DEPRECATED.
}
\examples{
tst <- rtPhase1Test()
}

