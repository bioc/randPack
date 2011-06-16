\name{ClinicalTrial-class}
\Rdversion{1.1}
\docType{class}
\alias{ClinicalTrial-class}

\title{Class "ClinicalTrial"  }
\description{	 container for clinical trial randomization infrastructure and patient data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ClinicalTrial", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{Experiment}:}{Object of class \code{"ClinicalExperiment"}. }
    \item{\code{PatientData}:}{Object of class \code{"environment"}. }
    \item{\code{Randomizers}:}{Object of class \code{"list"}, one
 randomizer for each strata. }
  }
}
\section{Methods}{
No methods defined with class "ClinicalTrial" in the signature.
}
\author{RG and VC }

\seealso{
\code{\link{ClinicalExperiment-class}}
}
\examples{
data(CT1)
CT1
}
\keyword{classes}
