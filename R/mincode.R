 

factorCounts = function (df, features, trtvec, obsdf, trttab) {
 # INPUT:
 # df is a Nxp data frame for enrolled cohort
 # features is a q-vector, q<p of variables in the data frame
 # trtvec is an N vector of allocations
 # obsdf is a data frame for an observation satisfying all(features %in%
 #    colnames(obsdf))
 # trttab is the treatment table 
 # OUTPUT: a list with R = length(unique(trtvec)) elements
 # the jth element of the list is a vector counting matches, to the
 #    data values in obsdf, present in the cohort members allocated
 #    to treatment j
 df = df[, features, drop=FALSE]
 sdf = split(df, trtvec)
 ac = as.character
 lapply(sdf, function(x) sapply(names(x), function(z) sum(ac(x[[z]])==ac(obsdf[[z]]))))
}

minimizeTaves = function(df, features, trtvec, obsdf, trttab) {
 picks = factorCounts(df, features, trtvec, obsdf)
 if (length(picks) < 2) return( sample(rep(names(trttab), times=trttab), size=1) )
 sums = sapply(picks,sum)
 names(sums)[which.min(sums)]
}

minimizePocSim = function(df, features, trtvec, obsdf, trttab, f=function(x,y) 
     sum(abs(x+1-y))) {
#
# mostly deterministic Pocock-Simon with unweighted sum
# -- if only one treatment has been seen, sample randomly from treatment table
#
 picks = factorCounts(df, features, trtvec, obsdf)
 if (length(picks)>2) stop("only works for two-arm studies")
 if (length(picks) < 2) {
       return(sample(rep(names(trttab), times=trttab), size=1))
       }
 sco1 = f(picks[[1]], picks[[2]])
 if (length(sco1)!=1 || !is.numeric(sco1)) stop("f must return numeric scalar")
 sco2 = f(picks[[2]], picks[[1]])
 ans = names(picks)
 ifelse(sco1>sco2, ans[2], ans[1])
}

