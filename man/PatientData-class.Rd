\name{PatientData-class}
\Rdversion{1.1}
\docType{class}
\alias{PatientData-class}

\title{Class "PatientData"}
\description{A class designed to hold patient data when randomizing
 participants to a clinical trial.} 
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PatientData", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{That patients name, a length one \code{character}
       vector. }
    \item{\code{covariates}:}{A named list, with one element for each variable.}
    \item{\code{date}:}{An instance of the \code{Date} class.}
    \item{\code{patientID}:}{A length one character vector containing the
      assigned patient ID.}
    \item{\code{strata}:}{A length one character vector describing the strata the patient will be randomized to.}
  }
}
\section{Methods}{
No methods defined with class "PatientData" in the signature.
}
\section{Description}{
This class is used to hold the internal representation of patient data.  
We chose to use a named list to hold the covariate information as it might
be relatively free form, with different centers (strata) providing different 
levels of detail.  Internal code will need to be careful to handle this 
properly (and it doesn't currently).

The \code{patientID} slot will hold the assigned patient ID, usually this
is provided by the software during randomization, but there are situations
(eg cross-over trials, or trials with specific requirements where this
might be pre-assigned) [current implementation does not handle this case].
}
\author{RG and VC }
\examples{
showClass("PatientData")
}
\keyword{classes}
