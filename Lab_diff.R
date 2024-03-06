V2 = 25
V

K = neldermead(c(1,1,1), min, c(0,0,0))

D <- function(t){
  dose <- 0
  return(dose)
}

f <- function(t, con, k) {
  equation <- array(0, c(2,1))
  equation[1] <- (D(t)/V1)-con[1]*(k[1]+k[2])+(k[3]*V2/V1*c[2])
  equation[2] <- (k[1]*V1/V2*con[1])-k[3]*con[2]
}



Time <- c(0, 5, 15, 30, 60, 120, 240, 480)
Blood_conc <- c(88, 9.54, 7.34, 6.44, 5.38, 4.18, 2.82, 1)
Tissue_conc <- c(0, 2.39, 4.35, 5.41, 5.47, 4.59, 3.13, 1.46)

Euler <- function(f, c0, t1, t2, tau) {
  time = seq(t1, t2, by=tau)
  c = array(0, c(length(c0), length(time)))
  c[,1] = c0
  for (i in 1:(length(time)-1)) {
    c[,i+1] = c[,i] + tau*f(time[i], c[,i])
  }
  return(c)
}

Heun <- function(f, c0, t1, t2, tau) {
  time = seq(t1, t2, by=tau)
  c = array(0, c(length(c0), length(time)))
  c[,1] = c0
  for (i in 1:(length(time)-1)) {
    k0 = f(time[i], c[,i])
    k1 = f(time[i+1],c[,i]+tau*k0)
    c[,i+1] = c[,i]+tau/2*(k0+k1)
  }
  return(c)
}

library(nloptr)
min = function(K) {
  t1 = Time[1]
  t2 = Time[8]
  c0 = #concentration from start
  h = #stepsize
  time = seq(t1, t2, by = h)
  c = Heun(f, c0, t1, t2, h, K) #Apply heun 
  timeRef = # the reference time
  cBloodRef = Blood_conc
  cTissueRef = Tissue_conc
  log = time %in% timeRef
  error1 = sum((c[1,log]-cBloodRef)^2)
  error2 = sum((c[2,log]-cTissueRef)^2)
  error = error1+error2
  return(error)
}

