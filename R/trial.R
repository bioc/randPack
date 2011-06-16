#
#setGeneric("setAllocations", function(tb, allocParms) 
# standardGeneric("setAllocations"))
#
#setClass("allocParms", representation(type="character", "VIRTUAL"))
#
#setClass("seedStruct", representation(startingState="integer",
#  endState="integer"))
#emptySeed = function() new("seedStruct")
#
#setClass("trialBase", 
#  representation(name="character", arms="character",
#    allocParms="allocParms", data="data.frame", seedData="seedStruct"))
# 
#
#setClass("unsetAllocParms", contains="allocParms")
#emptyAlloc = function() new("unsetAllocParms")
#
#setClass("permBlockParms", representation(blocksize="numeric",
#   nblocks="numeric"), contains="allocParms")
#
#setMethod("setAllocations", c("trialBase", "permBlockParms"),
#   function(tb, allocParms) {
#     require(combinat)
#     arms = tb@arms
#     bsize = allocParms@blocksize
#     narms = length(arms)
#     if (bsize %% narms) stop(paste("blocksize should be even multiple of number of arms, you have blocksize", bsize, "for",length(arms), "arms"))
#     fragsize = bsize/length(arms)
#     baseblock = rep(arms, each=fragsize)
#     allp = permn(baseblock)
#     pinds = 1:length(allp)
#     allpinds = pinds
#     while (length(allpinds) < allocParms@nblocks) 
#          allpinds = c(allpinds, pinds)
#     allpinds = allpinds[1:allocParms@nblocks]
#     initstate = .Random.seed
#     alloind = sample(allpinds, replace=FALSE)
#     finalstate = .Random.seed
#     alloc = unlist(allp[alloind])
#     if (length(tb@data) == 0 || prod(dim(tb@data)) == 0) tb@data = data.frame(alloc=alloc)
#     else tb@data = cbind(tb@data, alloc=alloc)
#     tb@allocParms = allocParms
#     tb@seedData = new("seedStruct", startingState=initstate,
#         endState=finalstate)
#     tb
#})
#
#pb4.8 = new("permBlockParms", type="PB", blocksize=4, nblocks=8)
#
#demoBase = new("trialBase", name="demo", arms=c("A", "B"),
#  allocParms=emptyAlloc(), seedData=emptySeed())
#
##demoBase = setAllocations(demoBase, pb4.8)
#
#setClass("efronBiasedCoinParms", representation(p="numeric", nsubj="numeric"), 
#   contains="allocParms")
#
##bcd.75 = new("efronBiasedCoinParms", p=.75, nsubj=32)
#
#setMethod("setAllocations", c("trialBase", "efronBiasedCoinParms"),
#  function(tb, allocParms) {
#    arms = tb@arms
#    p = allocParms@p
#    if (length(arms) != 2) stop("only functioning for 2 arm trials")
#    alloc = rep(NA, allocParms@nsubj)
#    initstate = .Random.seed
#    counts = c(0,0)
#    names(counts) = arms
#    alloc[1] = sample(arms, size=1)
#    counts[alloc[1]] = counts[alloc[1]] + 1
#    saveD = rep(NA, allocParms@nsubj)
#    saveD[1] = 0
#    for (j in 2:allocParms@nsubj) {
#       saveD[j] = D = counts[1] - counts[2]
#       if (D == 0) alloc[j] = sample(arms, size=1)
#          else if (D>0) alloc[j] = sample(arms, size=1, prob = c(1-p, p))
#          else if (D<0) alloc[j] = sample(arms, size=1, prob = c(p, 1-p))
#       counts[alloc[j]] = counts[alloc[j]] + 1
#       }
#    if (FALSE) plot(saveD)
#    finalstate = .Random.seed
#    tb@seedData = new("seedStruct", startingState=initstate,
#         endState=finalstate)
#    if (length(tb@data) == 0 || prod(dim(tb@data)) == 0) tb@data = data.frame(alloc=alloc)
#     else tb@data = cbind(tb@data, alloc=alloc)
#    tb
#})
#   
##dem2 = setAllocations( dem2, bcd.5 )
#
#setMethod("show", "trialBase", function(object) {
# cat("trial", object@name, "\n",
#     " arms:\n") 
#     print(object@arms)
# cat(" allocation method:", object@allocParms@type, "\n")
# cat(" study base dimensions: \n")
#     print(dim(object@data))
#})
#
