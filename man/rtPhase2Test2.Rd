\name{rtPhase2Test2}
\alias{rtPhase2Test2}
\title{a simple spatial tsetse population simulation, a 2nd test of phase2}
\usage{
rtPhase2Test2(nCol = 10, nRow = 10, pMove = 0.4, iDays = 4,
  iMaxAge = 7, iCarryCap = 200, iStartAdults = 200, iStartAges = 1,
  pMortF = 0.05, pMortM = 0.05, loop = TRUE)
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

  \item{pMortM}{adult male mortality per day}

  \item{loop}{TEMPORARY test of loop versus apply approach,
  only TRUE works}
}
\value{
a multi-dimensional array [day,x,y,sex,ages]
}
\description{
\code{rtPhase2Test2} runs a simple spatial popn simulation
as a test of phase 2 model components. Concentrates on
movement parameters and mortality so that it can be used to
test popn spread under different popn growth rates. Uses
things developed in gridTest.r. ! check if it works when
nRow & nCol == 1 (the aspatial model)
}
\examples{
tst <- rtPhase2Test2()
}

