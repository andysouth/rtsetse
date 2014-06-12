\name{rtPhase2Test}
\alias{rtPhase2Test}
\title{a simple spatial tsetse population simulation, a test of phase2}
\usage{
rtPhase2Test(nRow = 10, nCol = 10, pMove = 0.4, iDays = 4,
  iMaxAge = 120, iCarryCap = 200, iStartAdults = 200, iStartAges = 1,
  pMortF = 0.05)
}
\value{
a multi-dimensional array [day,x,y,ages] just for F to
start
}
\description{
\code{rtPhase2Test} runs a simple spatial popn simulation
as a test of phase 2 model components. Concentrates on
movement parameters and mortality so that it can be used to
test popn spread under different popn growth rates. Uses
things developed in gridTest.r. Starts with just F. ! check
if it works when nRow & nCol == 1 (the aspatial model)
}
\examples{
tst <- rtPhase2Test()
}

