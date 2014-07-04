\name{rtPhase2Test}
\alias{rtPhase2Test}
\title{DEPRECATED? first test of grid phase2 only has ageing and movement}
\usage{
rtPhase2Test(nRow = 10, nCol = 10, pMove = 0.4, iDays = 4,
  iMaxAge = 120, iCarryCap = 200, iStartAdults = 200, iStartAges = 1,
  pMortF = 0.05)
}
\arguments{
  \item{nCol}{number grid columns}

  \item{nRow}{number grid rows}

  \item{pMove}{probability of moving between cells}

  \item{iDays}{days to run simulation}

  \item{iMaxAge}{max age of fly allowed in model (will warn
  if flies age past this)}

  \item{iCarryCap}{carrying capacity of adults}

  \item{iStartAdults}{number of adults to start simulation
  with}

  \item{iStartAges}{spread start adults across the first n
  ages classes}

  \item{pMortF}{adult female mortality per day}
}
\value{
a multi-dimensional array [day,x,y,ages] just for F to
start
}
\description{
\code{rtPhase2Test} just does ageing & movement as a test
of phase 2 model components. The way it works has been
superceded by rtPhase2Test2(). Concentrates on movement
parameters and mortality so that it can be used to test
popn spread under different popn growth rates. Uses things
developed in gridTest.r. Starts with just F. ! check if it
works when nRow & nCol == 1 (the aspatial model)
}
\examples{
tst <- rtPhase2Test()
}

