##
## We used the SRS code, developed by Balasubramanian Narasimhan
## available at: http://r-forge.r-project.org/projects/srs/
## and the code developed by RG for the PRS system 
## as a starting point - this is heavily modified from those origins
## 
##
## factors is a list, one element for each factor;
##    the names of the list are the names of the factors;
##    the elements of the list are the levels of the factors
## patientIDs indicate the ids that can must be used.
## we need some way to describe strata - not yet implemented
## the randomization field should be a list of lists. One element of
## the top level list for each strata, then the randomizers are 
## represented in a list - this let's them change as the trial goes on;
## eg if a treatment is not available for a while

##patient IDs have a vector of strata names, starts and stops

setClass("PatientID",
         representation(strata = "character",
                        start="integer",
                        stop="integer")
         )
setMethod("show", "PatientID", function(object){
 cat("randPack PatientID instance for ", ns <- length(object@strata), " strata.\n")
 cat("The total number of IDs prepared is ", sum(sapply(1:ns, function(x)
   length(object@start[x]:object@stop[x]))), ".\n", sep="")
})

validPID = function(pID) {
    if( !is(pID, "PatientID") ) stop("need a PatientID instance")
    len = length(pID@strata)
    if( length(pID@start) != len || length(pID@stop) != len )
        stop("all slots must have the same length")
    if( len == 0 ) return(TRUE)
    ans = vector("list", length=len)
    for( i in 1:len) ans[[i]] = seq(pID@start[i], pID@stop[i])
    possSeqNames = unlist(ans)
    if(any(duplicated(possSeqNames)))
        stop("duplicated sequence names")
    return(TRUE)
}    


##the randomization slot contains a list, the names determine the
##strata and for each strata there is a list of randomizers (we need
##this second generalization to allow randomizers to change during the
##course of the trial.  In practice, some treatments may become
## unavailable, or may be contra-indicated, yet the trial will continue
## so we have to be able to change randomizers, and remove them,
## and potentially add the treatments back, once they are available

##the strataFun is a function that takes as input an instance of the
##PatientData class and returns a character string identifying the
##strata. 

setClass("ClinicalExperiment",
         representation(name ="character",
                        factors ="list",
                        treatments="integer",
                        randomization = "list",
                        strataFun = "function",
                        patientIDs="PatientID")
)
     
##for now no generics/methods, just a simple API

treatmentNames = function(object) names(object@treatments)
numberOfTreatments = function(object) length(object@treatments)

treatmentFactors = function(object) object@factors
factorNames = function(object) names(treatmentFactors(object))
numberOfFactorLevels = function(object) 
              sapply(treatmentFactors(object), length)

numberOfStrata = function(object) length(object@randomization)
strataNames = function(object) names(object@randomization)

setMethod("show", "ClinicalExperiment", 
      function(object) {
          cat("ClinicalExperiment: ", object@name, "\n")
          cat("  With", numberOfTreatments(object), "treatments\n")
          cat("        ", paste(selectSome(names(object@treatments)), collapse=" "), "\n")
          cat("  With", numberOfStrata(object), "strata\n")
          cat("        ", paste(strataNames(object), collapse=" "))
          cat("\n")
      }
)

    

##this class holds the data, the description of the trial and
## the randomizers (one for each strata)
setClass("ClinicalTrial",
       representation(Experiment = "ClinicalExperiment",
                      PatientData = "environment",
                      Randomizers = "list") 
)

##FIXME: need to have a better way of dealing with the randomizers, so that seeds
## start and stop dates are specified in the Desc object and then just used
## need to be able to have multiple randomizers/strata

createTrial = function(CExp, seed) {
          Denv = new.env(parent=emptyenv())
          Denv$count = 0   # does this get used?  VJC
          nstrata = numberOfStrata(CExp)
          Rands = vector("list", length=nstrata)
          names(Rands) = strataNames(CExp)
          if(length(seed) != length(Rands) ) stop("not enough seeds specified")
          names(seed) = names(Rands)  # so seed indexing works below
          for(i in names(Rands) )
              Rands[[i]] = list(makeRandomizer(i,
              CExp@randomization[[i]][[1]], seed=seed[i]))   # VC asks: why [[1]] here?
          new("ClinicalTrial", Experiment=CExp,
                  PatientData = Denv, 
                  Randomizers = Rands)
}


     
## we need both a description of a randomizer, as a class, and an instance of
## that randomizer, the treatments slot is a named integer vector, the names
## are the treatment names and the counts are relative numbers of each
## the type must be one of: "Random", "PermutedBlock", "Minimization",
##     "EfronBiasedCoin", "Urn"

setClass("RandomizerDesc",
    representation(treatments = "integer",
                   type = "character",
                   "VIRTUAL")
)

setClass("MinimizationDesc", contains="RandomizerDesc", 
           representation=representation(method="function",
                 featuresInUse="character"))
      
# we load the ForcedAllocDesc with tools that will make it function
# as a MinimizationDesc upon coercion
#
#setClass("ForcedAllocDesc", contains="MinimizationDesc",
#   representation=representation(forcedTreatments="character",
#     featuresInUse="character", method="function"))

setClass("PermutedBlockDesc",
       contains = "RandomizerDesc",
       representation = representation(numBlocks = "integer")
)

setClass("RandomDesc",
     contains = "RandomizerDesc",
     representation = representation(numPatients = "integer"))

setClass("UrnDesc",
     contains = "RandomizerDesc",
     representation = representation(numPatients = "integer", alpha="numeric", beta="numeric"))

setClass("EfronBiasedCoinDesc",
     contains = "RandomizerDesc",
     representation = representation(numPatients="integer", p = "numeric"))


## a virtual class that all randomizers can extend
## there are some variables that will be placed into the environment
## and they can be incremented/modified without affecting the instance
setClass("Randomizer",
      representation(name="character",
      treatmentTable = "integer",
      stateVariables = "environment",
      "VIRTUAL"))

  .Randomizertypes = c("Random", "PermutedBlock", "Minimization",
      "EfronBiasedCoin", "Urn", "ForcedAlloc")

  setClass("Random", contains="Randomizer")

  setClass("PermutedBlock", contains="Randomizer")

  setClass("Urn", contains = "Randomizer")

  setClass("Minimization", contains = "Randomizer")
  setClass("EfronBiasedCoin", contains = "Randomizer")


# .newPBlock pregenerates all allocations

.newPBlock = function(pbdesc) {
    if(!is(pbdesc, "PermutedBlockDesc")) stop("need a PermutedBlockDesc")
       trttable = pbdesc@treatments
       base = rep(names(trttable), times=trttable)
       x = matrix("", ncol=pbdesc@numBlocks, nrow=length(base))
       for(i in 1:pbdesc@numBlocks) x[,i] = sample(base)
       as.character(x)
}
 
.newRandom = function(rdesc) {
      trttable = rdesc@treatments
      sample(names(trttable), size=rdesc@numPatients, replace=TRUE, 
               prob = trttable/sum(trttable))
}

.newForced = function(rdesc) {
      rdesc@forcedTreatments
      }

.newEBC = function(rdesc) {
      if(!is(rdesc, "EfronBiasedCoinDesc")) stop("need an EfronBiasedCoinDesc") 
      if (length(rdesc@treatments) != 2) stop("EfronBiasedCoin randomization only implemented for 2 treatments")
      if (rdesc@treatments[1] != rdesc@treatments[2]) stop("EfronBiasedCoin randomization not implemented for unequal allocation")
      if (rdesc@p <= 0.5) stop("p must be in (0.5, 1.0)")
      if (rdesc@p >= 1.0) stop("p must be in (0.5, 1.0)")
      ebc = function(n, p, trts=c("A", "B")) {
# implement the basic Efron algorithm
        allocs = rep(trts[1], n)
        allocs[1] = sample(trts, prob=c(.5,.5), size=1)
        for (j in 2:n) {
#          print(D)
          D = 2*sum(allocs[1:(j-1)]==trts[1])-(j-1)
          if (D == 0) allocs[j] = sample(trts, size=1, prob=c(.5,.5))
          if (D < 0) allocs[j] = sample(trts, size=1, prob=c(p,1-p))
          if (D > 0) allocs[j] = sample(trts, size=1, prob=c(1-p,p))
          }
        allocs
       }
      ebc(rdesc@numPatients, rdesc@p, names(rdesc@treatments))
      }

.newUrn = function(rdesc) {
   if (!is(rdesc, "UrnDesc")) stop("need an UrnDesc")
   urnalloc = function(n, alpha, beta, trts=c("A", "B")) {
      allocs = rep(trts[1], n)
      allocs[1] = sample(trts, prob=c(.5,.5), size=1)
      for (j in 2:n) {
        NB = sum(allocs[1:(j-1)]==trts[2])
        p = (alpha + beta*NB)/(2*alpha + beta*(j-1))
        allocs[j] = sample(trts, size=1, prob=c(p,1-p))
        }
      allocs
    }
    urnalloc(rdesc@numPatients, rdesc@alpha, rdesc@beta, names(rdesc@treatments))
   }



##note that randomization takes place at the strata level;
##and it does seem that patient data needs to be stored at this level
##as well
makeRandomizer = function(name, type, seed){
# note that a RandomizerDesc is bound to 'type'
    if( !is(type, "RandomizerDesc") ) stop("need to specify the randomizer")
    Rand =  new(type@type, name=name,
        treatmentTable=type@treatments, stateVariables = new.env(parent=emptyenv()))
    Rand@stateVariables$seed = seed
    Rand@stateVariables$count = 1  # this quantity gives the index of the 'next' rand
    trts = NULL
    trts = switch(class(type), 
                  RandomDesc = .newRandom(type),
                  PermutedBlockDesc = .newPBlock(type),
                  EfronBiasedCoinDesc = .newEBC(type),
                  UrnDesc = .newUrn(type),
                  MinimizationDesc = {  # no precomputation of trts here...
                     Rand@stateVariables$method = type@method
                     Rand@stateVariables$featuresInUse = type@featuresInUse
                     NULL
                  },
                  ForcedAllocDesc = {
                        Rand@stateVariables$method = type@method
                        Rand@stateVariables$featuresInUse = type@featuresInUse
                        .newForced(type)
                        })
    Rand@stateVariables$treatments = trts
    Rand
}

##what level of copying is going on
##need to fix this up so it checks its args a lot better
addPData = function(cRand, pData) {
    if( !is(cRand, "Randomizer") )  stop("need a Randomizer")
    #cRand@stateVariables$pData = c(pData,cRand@stateVariables$pData)
    cRand@stateVariables$pData = c(cRand@stateVariables$pData,pData)
}


count = function(object)
    object@stateVariables$count

##not for export

trtAlloc = function(cRand, pData) {
    if( is(cRand, "Random")) {
        ct = count(cRand)
        trt = cRand@stateVariables$treatments[ct]  # all precomputed
        cRand@stateVariables$count = ct + 1
        }
    else if( is(cRand, "PermutedBlock")) {
        ct = count(cRand)
        trt = cRand@stateVariables$treatments[ct]  # all precomputed
        cRand@stateVariables$count = ct + 1
    }
    else if( is(cRand, "EfronBiasedCoin")) {
        ct = count(cRand)
        trt = cRand@stateVariables$treatments[ct]
        cRand@stateVariables$count = ct + 1
    }
    else if( is(cRand, "Urn")) {
        ct = count(cRand)
        trt = cRand@stateVariables$treatments[ct]
        cRand@stateVariables$count = ct + 1
    }
    else if( is(cRand, "ForcedAlloc")) {  # needs to precede Minimization
        # sorry, inefficient
        maxtrt = length(cRand@stateVariables$treatments)
        ct = count(cRand)
        if (ct > maxtrt) stop("All forced treatments specified in the ForcedAlloc description have been allocated.")
        trt = cRand@stateVariables$treatments[ct]
        cRand@stateVariables$count = ct + 1
    }
#    else if( is(cRand, "Urn")) {
#        trttable = cRand$stateVariables$treatments
#        base = rep(names(trttable), times=trttable)
#        trt = sample(base, 1)
#        cRand$stateVariable$treatments = trttable + cRand$stateVariables$add
#    }
    else if (is(cRand, "Minimization")) {
        ct = count(cRand)
        ttab = cRand@treatmentTable
        if (ct==1) {  # start things off
               trt = sample( rep(names(ttab), times=ttab), 1)
               }
        else {
               alld = cRand@stateVariables$pdata
               trt = cRand@stateVariables$method(alld, 
                        cRand@stateVariables$featuresInUse, alld$alloc, 
			pData@covariates, ttab) 
             }
        cRand@stateVariables$pdata = rbind(cRand@stateVariables$pdata,
                data.frame(pData@covariates, alloc=trt))  # this seems redundant with addPData -- is it?
        cRand@stateVariables$treatments = c(
            cRand@stateVariables$treatments, trt)
        cRand@stateVariables$count = ct + 1
    }
    return(trt)
}
        
##the basic idea is to get a new treatment; but this function must
##have side effects - the patient data has to be added to the current
##set of patient data and the count has to be incremented.
## and this is where environments really don't perform well, as we
##cannot easily get all updates made in a single commit - so it is
##relatively easy for the whole thing to get quite out of sync.  We
##need a more structured way to commit, or we need to think about a
##database, so we have some ability to control the resolution of
##transactions/commits.

getTreatment <- function(cTrial, pData) {
    if( !is(cTrial, "ClinicalTrial") ) stop("need a clinical trial")
    if( !is(pData, "PatientData")) stop("need a PatientData instance")
    ##start
    ##step 1 - select a randomizer: FIXME needs to be more general
    ## eg select according to strata or similar
    strata = cTrial@Experiment@strataFun(pData)
    rand = cTrial@Randomizers[[strata]][[1]]
    if( is.null(rand) ) stop("no appropriate randomizer list found")
    ##otherwise we go on
    trt = trtAlloc(rand, pData)
    addPData(rand, pData)
    return(trt)
}
     

##a class for Patient Data - just to formalize this
## we get their name (maybe), the randomization covariates,
## that come as a list so we can check them against the study design
## the variables used for stratification

setClass("PatientData", 
    representation(name="character",
          covariates = "list",
          date = "Date",   #date of randomization
          patientID = "character", 
          strata = "character"))


##FIXME: right now we are assuming 1 randomizer per strata
##  this gives us no option to turn treatments on and off
##  during a trial - not important for simulations, but
##  a problem if this will be used in practice
setMethod("show", "ClinicalTrial",
   function(object) {
 cat("randPack Clinical Trial instance\n")
 show(object@Experiment)
 randomizers= object@Randomizers
 for(i in names(randomizers)) {
  cat("  Randomizer: ", i , "\n")
  cat("    Patients Randomized", randomizers[[i]][[1]]@stateVariables$count-1, "\n")
}})

setGeneric("randomization<-", function(object, value)
   standardGeneric("randomization<-"))

# FIXME: still depending on only one element of list of randomizer descriptions per stratum
.checkRand2CT = function(ce, randdlist) {
  chkc = sapply(randdlist, function(x) is(x[[1]], "RandomizerDesc"))
  if (!all(chkc)) 
    stop("some element of list supplied for randomization<- is not a list of RandomizerDesc instance")
  if (length(randdlist) != length(ce@patientIDs@strata))
    stop("there must be one element of list supplied for randomization<- per stratum")
}

setMethod("randomization<-", c("ClinicalExperiment", "list"),
 function(object, value) {
   .checkRand2CT(object, value)
   object@randomization = value
   object
})
  
