\name{createTrial}
\Rdversion{1.1}
\alias{createTrial}

\title{
A function to create instances of the ClinicalTrial class.
}
\description{
This function should be used in preference to calls to \code{new} to create
instances of the ClinicalTrials class.
}
\usage{
createTrial(CExp, seed)
}
\arguments{
  \item{CExp}{An instance of the ClinicalExperiment class.}
  \item{seed}{A seed for the random number generator.}
}
\details{
To ensure proper initialization of the different components this
function should be called when creating new instances of the
\code{ClinicalTrial} class.
}
\value{ An instance of the \code{ClinicalTrial} class.
}
\author{
R. Gentleman
}

\seealso{
\code{\link{ClinicalExperiment-class}}
}
\examples{
 #define the available treatments and their relative allocations
 trts = c( A = 3L, B = 4L, C = 1L)
 #describe the permuted block design
 pbdesc = new("PermutedBlockDesc", treatments = trts, type="PermutedBlock",
     numBlocks=4L)

 pIDs = new("PatientID", strata="Test", start=100L, stop=200L)

 #define the ClinicalExperiment
 CE1 = new("ClinicalExperiment",
    name="My first experiment",
    treatments = trts,
    factors = list( F1 = c("A", "B", "C"), F2 = c("t1", "t2")),
    randomization = list(pbdesc),
    patientIDs = pIDs
 )


 #now create the instance of the trial
 CT1 = createTrial(CE1, seed=301)

}
