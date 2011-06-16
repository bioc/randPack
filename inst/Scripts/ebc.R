
ebc = function(n, p, trts=c("A", "B")) {
  allocs = rep(trts[1], n)
  allocs[1] = sample(trts, prob=c(.5,.5), size=1)
  for (j in 2:n) {
    print(D)
    D = 2*sum(allocs[1:(j-1)]==trts[1])-(j-1)
    if (D == 0) allocs[j] = sample(trts, size=1, prob=c(.5,.5))
    if (D < 0) allocs[j] = sample(trts, size=1, prob=c(p,1-p))
    if (D > 0) allocs[j] = sample(trts, size=1, prob=c(1-p,p))
    }
  allocs
}

table(ebc(100, 2/3))
 
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

table(urnalloc(100, 1, 3))
