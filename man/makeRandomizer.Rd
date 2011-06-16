\name{makeRandomizer}
\alias{makeRandomizer}
\title{
A function to create specific instances of randomizers.
}
\description{
This function should be used to create all randomizers. Direct calls to
\code{new} should not be used as some coordination etc is needed to 
create coherent and complete randomizers.
All of the information about the randomizer should be in the \code{type} 
argument which must be an instance of the \code{RandomizerDesc} class.
}
\usage{
makeRandomizer(name, type, seed)
}
\arguments{
  \item{name}{
The name of the randomizer.
}
  \item{type}{
An instance of the \code{RandomizerDesc} class. 
}
  \item{seed}{
The initial seed for the randomizer.
}
}
\details{
None right now.
}
\value{
An instance of the \code{Randomizer} class.
}
\author{
RG and VC
}

\seealso{
\code{\link{RandomizerDesc-class}}
}
\examples{
}
