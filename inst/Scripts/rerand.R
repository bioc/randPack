

library(randPack)
data(SampleData)


 pIDs = new("PatientID",
            strata = c("Center1", "Center2"),
            start = c(1000L, 2000L),
            stop = c(1150L, 2150L)
 )


 trts = c( A = 1L, B = 1L)
 pbdesc = new("PermutedBlockDesc", treatments = trts, type="PermutedBlock",
     numBlocks=400L)
 CE1 = new("ClinicalExperiment",
    name="My first experiment",
    treatments = trts,
    factors = list( F1 = c("A", "B", "C"), F2 = c("t1", "t2")),
    strataFun = function(pDesc) pDesc@strata,
    randomization = list(Center1= list(pbdesc), Center2=list(pbdesc)),
    patientIDs = pIDs
 )
  
 CE1
 CT1 = createTrial(CE1, seed=c(301, 401))
pdlist = lapply(1:nrow(SampleData), function(i)
 new("PatientData", date=Sys.Date(), name=paste("Patient", i, sep=""),
 covariates=SampleData[i, c("age", "sex", "surv")],
 strata=as.character(SampleData[i, "strata"])))
trts = lapply(1:nrow(SampleData), function(i) getTreatment(CT1, pdlist[[i]]))

fulld = getEnrolleeInfo(CT1)
snames = names(fulld)
fulldat = rbind( cbind(fulld[[1]], strat=snames[1]), cbind(fulld[[2]], strat=snames[2]))

library(survival)
obsModel = survreg(Surv(surv,rep(1,78))~age+sex+factor(alloc), data=fulldat)
obsStat = obsModel$coef[4]

NRERAND = 250
sds1 = round(10000*runif(NRERAND),0)
sds2 = round(10000*runif(NRERAND),0)
rerandStats = rep(NA, NRERAND)
for (i in 1:NRERAND) {
 CTn = createTrial(CE1, seed=c(sds1[i], sds2[i]))
 tmp = lapply(1:nrow(SampleData), function(i) getTreatment(CTn, pdlist[[i]]))
 rdat = getEnrolleeInfo(CTn)
 newdat = rbind( cbind(rdat[[1]], strat=snames[1]), cbind(rdat[[2]], strat=snames[2]))
 rerandStats[i] = survreg(Surv(surv,rep(1,78))~age+sex+factor(alloc), data=newdat)$coef[4]
}

print(summary(obsModel))
print(sd(rerandStats))
