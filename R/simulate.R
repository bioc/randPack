##factList is a list each component describes a factor
##  the name is the name of the factor, and value is either
## the proportions of the levels (named); or a function to simulate

 simPats <- function(npat, factList) {
      funs = sapply(factList, is.function)
      ans = lapply(factList, function(x) {
           if( is.function(x) ) x(npat) else
           sample(names(x), npat, replace=TRUE, prob=x)
      })
      as.data.frame(ans)
 }


