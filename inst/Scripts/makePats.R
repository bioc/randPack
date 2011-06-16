
 ##code to generate 60 or so patients:

 set.seed(123)

 name = c(paste("Sally ", LETTERS, ".", sep=""),
          paste("Bill", LETTERS, ".", sep=""),
          paste("Jose", LETTERS, ".", sep=""))

 sex = c(rep("F", 26), rep("M", 52))

 age = runif(78, min=21, max = 55)

 date = c(paste("2009","1", 1:26, sep="-"), paste("2009","2", 1:26, sep="-"),
           paste("2009", "3", 1:26, sep="-"))

 strata = sample(c("Center1", "Center2"), 78, replace=TRUE)

 trt = sample(c("A", "B"), 78, replace=TRUE)
 surv = ifelse(trt=="A", rexp(1, 1/10), rexp(1, 1/12))

 SampleData = data.frame(name=name, sex = sex, age=age, date=date, 
                     strata=strata, trt=trt, surv=surv)

 save(SampleData, file="SampleData.rda")

 


