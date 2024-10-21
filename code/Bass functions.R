#install.packages("minpack.lm")
#install.packages("nlstools")
#library(minplack.lm)
#library(nlstools)


# Bass ===========================================================================================
Bassnxt <- function(p=0.02, q=0.38, m=1, t=1:20, opt = 1) {
  # t = vector
  # opt = 1 for Nt, 2 for Xt as N(t) - N(t)-1 , 3 for Xt as dNt/dt
  if (opt == 3) {
    y <- m*(p+q)^2*exp(-(p+q)*t)/(1+q/p*exp(-(p+q)*t))^2  
  } else {
    Nt <- m*(1-exp(-(p+q)*t))/(1+q/p*exp(-(p+q)*t))
    Nt_1 <- m*(1-exp(-(p+q)*(t-1)))/(1+q/p*exp(-(p+q)*(t-1)))
    y <- Nt - (opt==2) * Nt_1
  }
  return(y)
}

BassNt <- function(p,q,m,t) m*(1-exp(-(p+q)*t))/(1+q/p*exp(-(p+q)*t))
BassdNt <- function(p,q,m,t) BassNt(p,q,m,t) - BassNt(p,q,m,t-1) 
BassNtSSE <- function(para,t,Nt) { -sum((Nt - BassNt(para[1],para[2],para[3],t))^2) } 
BassdNtSSE <- function(para,t,Xt) { -sum( (Xt - BassdNt(para[1],para[2],para[3],t))^2 ) }

BassGANLS <- function(t, y, opt=1) {
  if (opt == 1) {
    GA0  <- ga(type = "real-valued", fitness = BassNtSSE,
               t = t, Nt = y, lower = c(0, 0, max(y)), upper = c(10, 10, 1000*max(y)),
               popSize = 400, crossover = gareal_blxCrossover, maxiter = 5000, monitor = FALSE,
               run = 50, names = c("p", "q", "m"))
    GA.est <- summary(GA0)
    f <- BassNLS(t, y, p=GA.est$solution[1], q=GA.est$solution[2], m=GA.est$solution[3], opt=1)
  } 
  if (opt ==2) {
    GA0  <- ga(type = "real-valued", fitness = BassdNtSSE,
               t = t, Xt = y, lower = c(0, 0, sum(y)), upper = c(10, 10, 1000*sum(y)),
               popSize = 400, crossover = gareal_blxCrossover, maxiter = 5000, monitor = FALSE,
               run = 50, names = c("p", "q", "m"))
    GA.est <- summary(GA0)
    f <- BassNLS(t, y, p=GA.est$solution[1], q=GA.est$solution[2], m=GA.est$solution[3], opt=2)
  }
  return(f)
}

BassNLS <- function(t, y, p=0.02, q=0.38, m=1, opt=1) {
  d = data.frame(t, y)
  if (opt == 1) {
    Bass.est <- nlsLM(y ~ 0 + BassNt(p,q,m,t), data=d, 
                      start=list(p=p,q=q,m=m), algorithm = "port", lower = c(0, 0, max(y)),
                      control =  nls.control(maxiter = 200, tol = 1e-05, warnOnly = TRUE))
  }
  if (opt == 2) {
    Bass.est <- nlsLM(y ~ 0 + BassdNt(p,q,m,t), data=d, 
                      start=list(p=p,q=q,m=m), algorithm = "port", lower = c(0, 0, sum(y)),
                      control =  nls.control(maxiter = 200, tol = 1e-05, warnOnly = TRUE))
  }
  return(summary(Bass.est))
}

