\name{CT1}
\alias{CT1}
\alias{alltabs}
\alias{pD1}
\alias{sco}
\docType{data}
\title{
demonstration data for use with randPack
}
\description{
demonstration data for use with randPack
}
\usage{data(CT1)
data(SampleData)
data(alltabs)
data(pD1)
data(sco)
}
\format{
  The format for CT1 is is:
Formal class 'ClinicalTrial' [package "randPack"] with 3 slots \cr
  ..@ Experiment :Formal class 'ClinicalExperiment' [package "randPack"] with 6 slots \cr
  .. .. ..@ name         : chr "My first experiment" \cr
  .. .. ..@ factors      :List of 2 \cr
  .. .. .. ..$ F1: chr [1:3] "A" "B" "C" \cr
  .. .. .. ..$ F2: chr [1:2] "t1" "t2" \cr
  .. .. ..@ treatments   : Named int [1:3] 3 4 1 \cr
  .. .. .. ..- attr(*, "names")= chr [1:3] "A" "B" "C" \cr
  .. .. ..@ randomization:List of 2 \cr
  .. .. .. ..$ Center1:List of 1 \cr
  .. .. .. .. ..$ :Formal class 'PermutedBlockDesc' [package "randPack"] with 3 slots \cr
  .. .. .. .. .. .. ..@ numBlocks : int 4 \cr
  .. .. .. .. .. .. ..@ treatments: Named int [1:3] 3 4 1 \cr
  .. .. .. .. .. .. .. ..- attr(*, "names")= chr [1:3] "A" "B" "C" \cr
  .. .. .. .. .. .. ..@ type      : chr "PermutedBlock" \cr
  .. .. .. ..$ Center2:List of 1 \cr
  .. .. .. .. ..$ :Formal class 'RandomDesc' [package "randPack"] with 3 slots \cr
  .. .. .. .. .. .. ..@ numPatients: int 100 \cr
  .. .. .. .. .. .. ..@ treatments : Named int [1:3] 3 4 1 \cr
  .. .. .. .. .. .. .. ..- attr(*, "names")= chr [1:3] "A" "B" "C" \cr
  .. .. .. .. .. .. ..@ type       : chr "Random" \cr
  .. .. ..@ strataFun    :function (pDesc)   \cr
  .. .. .. ..- attr(*, "source")= chr "function(pDesc) pDesc@strata" \cr
  .. .. ..@ patientIDs   :Formal class 'PatientID' [package "randPack"] with 3 slots \cr
  .. .. .. .. ..@ strata: chr [1:2] "Center1" "Center2" \cr
  .. .. .. .. ..@ start : int [1:2] 1000 2000 \cr
  .. .. .. .. ..@ stop  : int [1:2] 1150 2150 \cr
  ..@ PatientData:<environment: 0x101c86590>  \cr
  ..@ Randomizers:List of 2 \cr
  .. ..$ Center1:List of 1 \cr
  .. .. ..$ :Formal class 'PermutedBlock' [package "randPack"] with 3 slots \cr
  .. .. .. .. ..@ name          : chr "Center1" \cr
  .. .. .. .. ..@ treatmentTable: Named int [1:3] 3 4 1 \cr
  .. .. .. .. .. ..- attr(*, "names")= chr [1:3] "A" "B" "C" \cr
  .. .. .. .. ..@ stateVariables:<environment: 0x101c867c0>  \cr
  .. ..$ Center2:List of 1 \cr
  .. .. ..$ :Formal class 'Random' [package "randPack"] with 3 slots \cr
  .. .. .. .. ..@ name          : chr "Center2" \cr
  .. .. .. .. ..@ treatmentTable: Named int [1:3] 3 4 1 \cr
  .. .. .. .. .. ..- attr(*, "names")= chr [1:3] "A" "B" "C" \cr
  .. .. .. .. ..@ stateVariables:<environment: 0x101c86a60>  \cr
}
\details{
various items used in examples or vignette have been serialized for
convenience.
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(CT1)
## maybe str(CT1) ; plot(CT1) ...
}
\keyword{datasets}
