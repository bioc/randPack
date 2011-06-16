

library(randPack)

pidStruct = new("PatientID",   # establish form and limits of IDs
        strata=c("C1", "C2", "C3", "C4"),
        start = c(1000L, 2000L, 3000L, 4000L),
        stop = c(1999L, 2999L, 3999L, 4999L))
pidStruct

ce = new("ClinicalExperiment",
           name="bookdemo",
           factors=list(
             center=c("C1", "C2", "C3", "C4"),
             sex=c("Male", "Female"),
             age="numeric"),
           treatments=c(A=2L, B=2L),
           randomization=list(),
              # following operates on PatientData instance
           strataFun = function(x)x@strata,
           patientIDs = pidStruct)

rdesc = new("RandomDesc",
       treatments = ce@treatments,
       type = "Random",
       numPatients = 1000L)

pbdesc = new("PermutedBlockDesc", treatments = c(A=2L,B=2L), type="PermutedBlock",
     numBlocks=1000L)


ce@randomization = list(C1=list(pbdesc),
                        C2=list(pbdesc),
                        C3=list(pbdesc),
                        C4=list(pbdesc))

CT1 = createTrial(ce, seed=c(201,101,501,601))
CT1

coh1 =  list(center=c(C1=.4, C2=.2, C3=.1, C4=.3),
             sex=c(Male=.5, Female=.5),
             age = function(x) runif(x, min=50, max=70))
simDat = simPats(1000, coh1)
simDat[1:5,]


pdlist = lapply(1:1000, function(i)
 new("PatientData", date=Sys.Date(), name=paste("Patient", i, sep=""),
 covariates=simDat[i, c("age", "sex")],
 strata=as.character(simDat[i, "center"])))
trts = lapply(1:1000, function(i) getTreatment(CT1, pdlist[[i]]))

ee = getEnrolleeInfo(CT1)

eee = ee[[1]]
for (i in 2:4) eee = rbind(eee,ee[[i]])
table(eee$alloc)
table(rle(as.character(eee$alloc))$len)
#
# quiz question -- how can you get a run of length 5 in this application?
# it is not a bug! 
#

#
# illustrate simple randomization
#

ce@randomization = list(C1=list(rdesc),
                        C2=list(rdesc),
                        C3=list(rdesc),
                        C4=list(rdesc))

CT2 = createTrial(ce, seed=c(201,101,501,601))
CT2
trts = lapply(1:1000, function(i) getTreatment(CT2, pdlist[[i]]))
ff = getEnrolleeInfo(CT2)
fff = ff[[1]]
for (i in 2:4) fff = rbind(fff,ff[[i]])
table(fff$alloc)
table(rle(as.character(fff$alloc))$len)

#
# illustrate efron's biased coin
#


ebcdesc = new("EfronBiasedCoinDesc",
       treatments = ce@treatments,
       type = "EfronBiasedCoin",
       numPatients = 1000L, p=2/3)

ce@randomization = list(C1=list(ebcdesc),
                        C2=list(ebcdesc),
                        C3=list(ebcdesc),
                        C4=list(ebcdesc))

CT3 = createTrial(ce, seed=c(201,101,501,601))
CT3
trts = lapply(1:1000, function(i) getTreatment(CT3, pdlist[[i]]))
gg = getEnrolleeInfo(CT3)
ggg = gg[[1]]
for (i in 2:4) ggg = rbind(ggg,gg[[i]])
table(ggg$alloc)
table(rle(as.character(ggg$alloc))$len)

#
# illustrate the urn design
#

urndesc = new("UrnDesc",
       treatments = ce@treatments,
       type = "Urn",
       numPatients = 1000L, alpha=1, beta=3)

ce@randomization = list(C1=list(urndesc),
                        C2=list(urndesc),
                        C3=list(urndesc),
                        C4=list(urndesc))

CT4 = createTrial(ce, seed=c(201,101,501,601))
CT4
trts = lapply(1:1000, function(i) getTreatment(CT4, pdlist[[i]]))
hh = getEnrolleeInfo(CT4)
hhh = hh[[1]]
for (i in 2:4) hhh = rbind(hhh,hh[[i]])
table(hhh$alloc)
table(rle(as.character(hhh$alloc))$len)
