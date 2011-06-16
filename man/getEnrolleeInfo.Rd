\name{getEnrolleeInfo}
\Rdversion{1.1}
\alias{getEnrolleeInfo}
\title{
Returns a named list, data frames by strata giving all covariate information
along with allocations
}
\description{
return data frames by strata giving all covariate information
along with allocations
}
\usage{
getEnrolleeInfo(x)
}
\arguments{
  \item{x}{An instance of the \code{\link{ClinicalTrial-class}}
}
}
\value{
A list of data frames, one per stratum, for all current enrollees.
The first column is named \code{name} and gives the patient name, the
last column is named \code{alloc} and gives the treatment allocation
for that patient.
}

\author{
RG and VC
}

\examples{
data(CT1)
getEnrolleeInfo(CT1)
}
\keyword{ models }
