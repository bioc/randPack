\name{Randomizer-class}
\Rdversion{1.1}
\docType{class}
\alias{Randomizer-class}
\alias{ForcedAlloc-class}
\alias{Random-class}
\alias{PermutedBlock-class}
\alias{Urn-class}
\alias{coerce,Randomizer,Minimization-method}
\alias{RandomizerDesc-class}
\alias{ForcedAllocDesc-class}
\alias{RandomDesc-class}
\alias{PermutedBlockDesc-class}
\alias{UrnDesc-class}

\title{Class "Randomizer" and "RandomizerDesc" plus their subclasses.}
\description{These classes form the infrastructure that can be used to 
create different randomizers for clinical trials or other similar treatment
allocation experiments.}
\section{Objects from the Class}{
Both "Randomizer and "RandomizerDesc" are virtual classes and
no objects may be created from them. Their various subclasses, 
"ForcedAlloc", "Random", "PermutedBlock" and "Urn" can be instantiated.
}
\section{Slots}{
For "RandomizerDesc":
\describe{
  \item{\code{treatments}:}{A named integer vector.  The names correspond to
   treatment names, the integers are relative allocations.}
  \item{\code{type}:}{The name of the randomizer, this will be set internally.}
}
For "Randomizer":
  \describe{
    \item{\code{name}:}{The name of the randomizer.}
    \item{\code{treatmentTable}:}{A named integer vector. The names
  correspond to treatment names and the integer values to relative allocations.}
    \item{\code{stateVariables}:}{An environment that is used to hold any
    variables that need to retain state information.}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "Randomizer", to = "Minimization")}: ... }
	 }
}
\author{RG and VC}

\examples{
showClass("Randomizer")
}
\keyword{classes}
