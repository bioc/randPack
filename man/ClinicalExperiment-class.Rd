\name{ClinicalExperiment-class}
\Rdversion{1.1}
%\docType{class}
\alias{ClinicalExperiment-class}
\alias{show,ClinicalExperiment-method}
\alias{randomization<-,ClinicalExperiment,list-method}
\alias{randomization<-}

\title{Class "ClinicalExperiment" }
\description{A class that defines the parameters for a clinical trial.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ClinicalExperiment", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{A string naming the experiment. }
    \item{\code{factors}:}{A named list, the names are the names of the
      factors that will be used for randomization. The elements of the
      list are the names of the factor levels.
    }
    \item{\code{treatments}:}{A named integer vector indicating the
      relative frequency of treatment assignments that are desired.
      The names are the treatment names.}
   \item{\code{strataFun}:}{A function that can be applied to instances
      of \code{PatientData} to obtain the strata that the patient is to
      be entered on.}
   \item{\code{randomization}:}{A list of the randomizers to be used.
      There is one for each strata.}
   \item{\code{patientIDs}:}{An instance of the \code{PatientID} class
    that is used to assign patient IDs to the individuals as they are
    randomized into the trial.}  
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "ClinicalExperiment")}: a print
    method.}
    \item{randomization<-}{\code{signature(object = "ClinicalExperiment", value="list")}: binds a list of lists of randomizer descriptions to a clinical experiment
instance }
	 }
}
\section{Description}{
  Basically the ClinicalExperiment class should contain the static
  information that describes a clinical trial and in particular how
  patients will be randomized to treatments in that trial.

  There is a name for the trial, then a list of the factors that can be
  used for randomization.  We have yet to decide on how to handle
  continuous variables (these can be used in some randomization
  schemes).  We also have not yet included strata.

  Most of the accessors are simple functions, not methods. 
}
\author{R. Gentleman }
\note{ Similar to the classes defined by N. Balasubraminian. }


\seealso{
  \code{\link{treatmentNames}}, \code{\link{numberOfTreatments}},
  \code{\link{factorNames}}
}
\examples{
showClass("ClinicalExperiment")
data(CT1)
CT1@Experiment
}
\keyword{classes}
