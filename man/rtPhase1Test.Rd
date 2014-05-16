\name{rtPhase1Test}
\alias{rtPhase1Test}
\title{an a-spatial tsetse population simulation, a test of phase1}
\usage{
rtPhase1Test(iDays = 30, iMaxAge = 120, iCarryCap = 200,
  iStartAdults = 200, iStartAges = 1, pMortF = 0.05, pMortM = 0.05,
  pMortPupa = 0.25, propMortPupaDD = 0.25, iPupDurF = 26, iPupDurM = 28,
  iFirstLarva = 16, iInterLarva = 10, pMortLarva = 0.05,
  propMortLarvaDD = 0.25, plot = FALSE, verbose = TRUE)
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

  \item{pMortF}{adult female mortality per day}

  \item{pMortM}{adult male mortality per day}

  \item{pMortPupa}{pupal mortality per period}

  \item{propMortPupaDD}{proportion of pupal mortality that
  is density dependent}

  \item{iPupDurF}{days it takes pupa(F) to develop}

  \item{iPupDurM}{days it takes pupa(M) to develop}

  \item{iFirstLarva}{Age that female produces first larva}

  \item{iInterLarva}{Inter-larval period}

  \item{pMortLarva}{larval mortality per period}

  \item{plot}{whether to plot graphs}

  \item{verbose}{whether to output progress messages to
  console}
}
\value{
a list of lots, initially dataframes of adult age structure
over time dfRecordF & M
}
\description{
\code{rtPhase1Test} runs an a-spatial popn simulation as a
test of phase 1 model components. It uses default parameter
values that are mostly similar to hat-trick.
}
\examples{
rtPhase1Test()
}

