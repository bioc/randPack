\name{minimizePocSim}
\alias{minimizePocSim}
\alias{minimizeTaves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
use the Pocock-Simon or Taves algorithm for computing covariate-adaptive
'minimization' allocations
for a clinical trial
}
\description{
use the Pocock-Simon or Taves algorithm for computing covariate-adaptive
'minimization' allocations
for a clinical trial
}
\usage{
minimizePocSim(df, features, trtvec, obsdf, trttab, f = function(x, y) sum(abs(x + 1 - y)))
minimizeTaves(df, features, trtvec, obsdf, trttab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame with columns corresponding to covariates
rows corresponding to subjects
}
  \item{features}{
character vector of covariates to use
}
  \item{trtvec}{
vector of assignments made so far
}
  \item{obsdf}{
data frame for incoming observation, with values for
all components enumerated in \code{features}
}
  \item{trttab}{
table of treatment ratios
}
  \item{f}{
score that determines impending allocation
}
}
\details{
These functions are generally not called directly.
See the vignette; if supplied as the method slot of
a MinimizationDesc object the appropriate data are
assembled as arguments to these functions.
}
\value{
a treatment code
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
new("MinimizationDesc", treatments=c(A=1L, B=1L), method=minimizePocSim,
   type="Minimization", featuresInUse="sex")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
