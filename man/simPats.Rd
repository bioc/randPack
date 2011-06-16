\name{simPats}
\alias{simPats}
\title{
A function to simulate patient covariate data.
}
\description{
The function allows for a relatively simple description of covariates for
patients in a clinical trial and then the subsequent simulation of a 
cohort of the necessary size.
}
\usage{
simPats(npat, factList)
}
\arguments{
  \item{npat}{The number of patients to simulate.}
  \item{factList}{ A list containing either functions that will
  be called or named vectors containing relative proportions, for
  factors.}
}
\value{
 A data.frame with \code{npat} rows and one column for each variable
given in the \code{factList}.
}
\author{
RG and VC
}

\examples{
coh1 =  list(center=c(C1=.4, C2=.2, C3=.1, C4=.3),
             sex=c(Male=.5, Female=.5),
             age = function(x) runif(x, min=50, max=70))
simDat = simPats(100, coh1)
simDat[1:5,]
table(simDat[,1])
}
