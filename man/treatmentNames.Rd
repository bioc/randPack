\name{treatmentNames}
\Rdversion{1.1}
\alias{treatmentNames}
\alias{numberOfTreatments}
\alias{treatmentFactors}
\alias{factorNames}
\alias{numberOfFactorLevels}
\title{
Accessor functions for the ClinicalExperiment class.
}
\description{
  These functions should be used to access the information in a
  instance of the \code{\link{ClinicalExperiment-class}} class.
}
\usage{
treatmentNames(object)
numberOfTreatments(object)
treatmentFactors(object)
factorNames(object)
numberOfFactorLevels(object)
}
\arguments{
  \item{object}{
An instance of the ClinicalExperiment class.
}
}
\details{
These functions provide the names of treatments, the names of factors,
and the levels of the factors.
}
\value{
In all cases the information indicated by the name of the function is
returned, in an appropriate form.
}

\author{
R. Gentleman
}

\examples{
data(CT1)
treatmentNames(CT1@Experiment)
}
