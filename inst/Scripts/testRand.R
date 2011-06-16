
  library("randPack")

  trts = c( A = 3L, B = 4L, C = 1L)

 pbdesc = new("PermutedBlockDesc", treatments = trts, type="PermutedBlock",
     numBlocks=4L)

  ##should be 24 long and have one C ever 8 allocs
  randPack:::.newPBlock(pbdesc)



  rd = new("RandomDesc", treatments = trts, type = "Random", 
             numPatients = 100L)

  mmpb = makeRandomizer("Expt1", pbdesc, seed = 101)

  mmr = makeRandomizer("Expr1r", rd, seed=201)

  ##since the strata are Center1 or Center2, we can just pull those
  ##out directly. If the strata were by, say sex and center then we
  ##would need to have some name mangling scheme.

  sFun = function(pDesc) pDesc@strata

## patientID class

 pIDs = new("PatientID",
            strata = c("Center1", "Center2"),
            start = c(1000L, 2000L),
            stop = c(1150L, 2150L)
 )

 validPID(pIDs)

##define an experiment with two strata
 CE1 = new("ClinicalExperiment",
    name="My first experiment",
    treatments = trts,
    factors = list( F1 = c("A", "B", "C"), F2 = c("t1", "t2")),
    strataFun = sFun,
    randomization = list(Center1= list(pbdesc), Center2=list(rd)),
    patientIDs = pIDs
 )
  
 CE1

 treatmentFactors(CE1)
 factorNames(CE1)
 numberOfFactorLevels(CE1)


 CT1 = createTrial(CE1, seed=c(301, 401))

 save(CT1, file = "CT1.rda")
 
 CT1

 ##now how to randomize patients


 pD1 = new("PatientData", name="Sally H", date=Sys.Date(),
 covariates=list(sex="F", age=33), strata="Center1")

 save(pD1, file = "pD1.rda")

 CE1@strataFun(pD1)

 trt = getTreatment(CT1, pD1)

 ## and now what has happened...
 ## we should see that one 
 CT1
