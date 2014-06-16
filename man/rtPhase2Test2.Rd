\name{rtPhase2Test2}
\alias{rtPhase2Test2}
\title{a simple spatial tsetse population simulation, a 2nd test of phase2}
\usage{
rtPhase2Test2(nRow = 10, nCol = 10, pMove = 0.4, iDays = 4,
  iMaxAge = 7, iCarryCap = 200, iStartAdults = 200, iStartAges = 1,
  pMortF = 0.05, pMortM = 0.05, loop = TRUE)
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

