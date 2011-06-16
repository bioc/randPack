\name{getTreatment}
\Rdversion{1.1}
\alias{getTreatment}
\title{
Computes random allocation to treatment of a patient in a trial
}
\description{
Computes random allocation to treatment of a patient in a trial
according to the randomizers etc. defined.
}
\usage{
getTreatment(cTrial, pData)
}
\arguments{
  \item{cTrial}{An instance of \code{\link{ClinicalTrial-class}.}
}
  \item{pData}{An instance of \code{\link{PatientData-class}.}
}
}
\details{
}
\value{
}
\author{
RG and VC
}
\seealso{
\code{\link{help}}
}
\examples{
data(pD1)
data(CT1)
getEnrolleeInfo(CT1)
getTreatment(CT1, pD1)
getEnrolleeInfo(CT1)
}
