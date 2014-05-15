\name{rtPupalEmergence}
\alias{rtPupalEmergence}
\title{tsetse pupal emergence}
\usage{
rtPupalEmergence(vPupaF, vPupaM, vPopF, vPopM)
}
\arguments{
  \item{vPupaF}{a vector of pupal Females by age}

  \item{vPupaM}{a vector of pupal Males by age}

  \item{vPopF}{a vector of adult Females by age}

  \item{vPopM}{a vector of adult Males by age}
}
\value{
a list containing vPupaF,vPupaM,vPopF & vPopM
}
\description{
\code{rtPupalEmergence} returns the number of emerging pupa
}
\details{
pupal emergence needs to come from the number of pupae in
the ground of the appropriate age

Initially just gets the number of pupae from the final age
class of the passed vector. Ideally I would like to set the
number in that final age class to 0 at the same time
However I can't easily pass 2 objects back ... Would be
less code if not in a function ... Want to allow for it
being more complex in future ... Can get it to return
vectors of both pupae & adults
}

