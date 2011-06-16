
getCovariates = function (x) 
{
    tmp = lapply(x@Randomizers, function(z) 
            lapply(z, function(w) 
              lapply(w@stateVariables$pData, slot, "covariates")))
    lapply( tmp, function(x) {
       lapply(x, function(z) {
             if (length(z) > 1) {
                bdf = z[[1]]
                for (i in 2:length(z)) bdf = rbind(bdf, z[[i]])
                return(bdf)
             } else return(z) } ) } )
}



#getEnrolleeInfo = function (x) 
#{
## assumes x is ClinicalTrial instance
#if (is(x, "Minimization")) tmp = 
#       list(list(lapply(x[[1]][[1]]@stateVariables$pData, 
#            function(x)  slot(x, "covariates"))))
#else tmp = lapply(x@Randomizers, function(z) 
#            lapply(z, function(w) 
#              lapply(w@stateVariables$pData, slot, "covariates")))
#if (is(x, "Minimization")) trts = 
#   list(list(x[[1]][[1]]@stateVariables$treatments))
#else trts = lapply(x@Randomizers, function(z) 
#            lapply(z, function(w) 
#              get("treatments", w@stateVariables)))
#    ans = lapply( tmp, function(x) {
#       lapply(x, function(z) {
#             if (length(z) > 1) {
#                bdf = z[[1]]
#                for (i in 2:length(z)) bdf = rbind(bdf, z[[i]])
#                return(bdf)
#             } else return(z) } ) } )
#    for (i in 1:length(ans)) {
#        for (j in 1:length(ans[[i]]))
#           ans[[i]][[j]] = cbind(ans[[i]][[j]], alloc=trts[[i]][[j]][1:
#             nrow(ans[[i]][[j]])])
#        }
#    ans
#}

summarizeStateVars = function(x) {
  ##x is an environment containing pData and treatments
  name = sapply(x$pData, function(w) w@name)
  covs = lapply(x$pData, function(w) data.frame(w@covariates))
  if( length(covs) > 1 ) covs = do.call(rbind, covs)
  alloc = x$treatments[seq_along(name)]
  cbind(name, covs, alloc)
}

##FIXME: RG is not so sure that Minimization deserves such special treatment
## RIGHT!
getEnrolleeInfo = function(x) {
#  if(is(x, "Minimization")) 
#     list(summarizeStateVars(x@stateVariables))
#  else {
    if(!is(x, "ClinicalTrial")) 
           stop("only clinical trials instances, please")
    ans = lapply(x@Randomizers, function(y) 
                 summarizeStateVars(y[[1]]@stateVariables))
    names(ans) = names(x@Randomizers)
    return(ans)
#  }
}
