\name{PatientID-class}
\Rdversion{1.1}
\docType{class}
\alias{PatientID-class}
\alias{validPID}

\title{Class "PatientID"}
\description{A class to represent ID strings that will be assigned to
  patients as they are randomized in a clinical trial.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PatientID", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{strata}:}{A vector of strata identifiers. }
    \item{\code{start}:}{The starting indices for each strata.}
    \item{\code{stop}:}{The largest value for each strata.}
  }
}
\section{Methods}{
No methods defined with class "PatientID" in the signature.
}
\section{Description}{
Generally patient IDs are assigned at the time a patient is randomized.
The scheme used here is very simple, disjoint sets of integers are used
for the different strata.  

Once all patiend IDs have been allocated further attempts to randomize
patients on that strata should fail.
}
\author{RG and VC}
\examples{
 pIDs = new("PatientID",
            strata = c("Center1", "Center2"),
            start = c(1000L, 2000L),
            stop = c(1150L, 2150L)
 )
 validPID(pIDs)
}
\keyword{classes}
