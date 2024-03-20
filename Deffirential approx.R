Euler <- function(f, c0, t1, t2, tau){
  time = seq(t1, t2, by=tau)
  c = array(0, c(1, length(time)))
  c[1] = c0
  for (i in 1:(length(time)-1)) {
    c[i+1] = c[i]+tau*f(time[i], c[i])
  }
  return(c)
}

heun = function(f, c0, t1, t2, tau){
  time = seq(t1, t2, by=tau)
  c= array(0, c(1, length(time)))
  c[1] = c0
  for (i in 1:(length(time)-1)) {
    k0 = f(time[i], c[i])
    k1 = f(time[i+1], c[i]+tau*k0)
    c[i+1] = c[i]+tau/2*(k0+k1)
  }
  return(c)
}

D = function(t){
  if((t%%10)<0.5) {
    D=2
  } else {
    D= 0
  }
}

f = function(t,c) {
  f = D(t)/9-0.41187*c
}

t1 = 0; t2 = 48
c0 = 0; tau = 0.1
c1 = Euler(f, c0, t1, t2, tau)
c2 = heun(f, c0, t1, t2, tau)

### compartment metode ----

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
    k1 = f(time[i+1], c[,i] + tau*k0)
    c[,i+1] = c[,i] + tau/2*(k0+k1)
  }
  return(c)
}

f <- function(t,s){
  k1=1
  k2 = 1
  k3 = 1
  k4 = 1
  k5 = 1
  P = 0.5
  f = array(0, c(3,1))
  f[1] = (k1*P-k4)*s[1] - k2*s[1]*s[2]
  f[2] = k5*s[3]-k3*s[2]
  f[3] = k4*s[1]-k5*s[3]
  
  return(f)
}

t1 = 0; t2 = 48
c0 = c(3,0,0); tau = 0.1
c1 = Euler(f,c0,t1,t2,tau)
c2 = Heun(f, c0, t1, t2, tau)

## Kunge-rutta ----


f <- function(t, s) {
  
# hvis s0 konstant så v0 = 10
#v1 = 10 + c1*(s[2]-s[1])
  
  v1 = c1*(s[2]-s[1])
  v2 = c2*s[2]
  v3 = c3 * s[2]
  v4 = c4*s[4]
  v5 = c5*s[3]
  f = array(0, c(5,1))
  f[1] = v1
  f[2] = -v1 - v2 - v3
  f[3] = v2 + v4 - v5
  f[4] = v3 - v4
  f[5] = v5
  
  return(f)
}

rk4 <- function(f, y0, t1, t2, tau) {
  time = seq(t1, t2, by = tau)
  y = array(0, c(length(y0), length(time)))
  y[,1] = y0
  for (i in 1:(length(time) - 1)) {
    k1 = f(time[i], y[,i])
    k2 = f(time[i] + tau/2, y[,i] + tau/2 * k1)
    k3 = f(time[i] + tau/2, y[,i] + tau/2 * k2)
    k4 = f(time[i] + tau, y[,i] + tau * k3)
    y[,i + 1] = y[,i] + tau/6 * (k1 + 2*k2 + 2*k3 + k4)
  }
  
  return(y)
}

c1 = 1.1; c2 = 0.1; c3 = 0.1; c4 = 0.3; c5 = 1.0
t1 = 0; t2 = 100
tau = 0.1
v0 = 10
y0 = c(10,0,0,0, 0)
y1 = rk4(f, y0, t1, t2, tau)
plot(y1[1,])
plot(y1[2,])
plot(y1[3,])
plot(y1[4,])
plot(y1[5,])

time = seq(t1, t2, by = tau)

plot(time, y1[1,], type = "l", col = "green")
lines(time ,y1[2,], type = "l", col = "red")
lines(time, y1[3,], type = "l", col = "blue")
lines(time, y1[4,], type = "l", col = "orange")
lines(time, y1[5,], type = "l", col = "purple")


par(mfrow = c(2, 3))

## Opgave 1 ----

