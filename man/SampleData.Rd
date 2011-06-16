\name{SampleData}
\alias{SampleData}
\docType{data}
\title{
Randomly generated data representing a potential patient cohort.
}
\description{
The data were generated using the script in
\code{inst/scripts/makePats.R}
and are intended for testing components of the package.
}
\usage{data(SampleData)}
\format{
  A data frame with 78 observations on the following 7 variables.
  \describe{
    \item{\code{name}}{The patients name.}
    \item{\code{sex}}{The sex of the patient, \code{M} or \code{F}.}
    \item{\code{age}}{The age of the patient.}
    \item{\code{date}}{The date they were entered into the trial.}
    \item{\code{strata}}{Which center they were entered by,
    \code{Center1} or \code{Center2}. }
    \item{\code{trt}}{A treatment label, either \code{A} or \code{B}.}
    \item{\code{surv}}{Survival time, if treatment is \code{A} then
    exponential with a mean of 10, for \code{B} exponential with mean 12.}
  }
}
\details{
Clearly the randomizer will assign the patients to treatments, so the
indicator here is irrelevant, but it seemed useful to have some
difference in survival, and to keep track of it.
}
\examples{
data(SampleData)
SampleData[1:10,]
}
\keyword{datasets}
