getwd()
setwd("your_path")

# Startup MAU Bass estimation 

source("Bass functions.R")
source("tg_base_functions.R")

#library(GA)
#library(readr)
#library(minpack.lm)

d <- read.csv("rawdata_ex2.csv", header=T)

resultM <- matrix(0, 100, dim(d)[2]*2)

for (comi in 2:dim(d)[2]) {
  y <- as.numeric(d[,comi])
  y <- y[y > 0]
  x <- diff(y)
  t <- 1:length(y)
  
  esti <- BassNLS(t=t, y=y, opt=1)
  p.esti <- esti$coefficients[,1]
  yhati <- BassNt(p.esti[1], p.esti[2], p.esti[3], t)
  
  resultM[1:(length(y)+10), ((comi-1)*2+1):((comi-1)*2+2)] <- 
    rbind(esti$coefficients[, c(1,4)], t(fitness(y, yhati ,3)), cbind(y, yhati))
  
}

write.csv(resultM, "result_ex2_2.csv")



#roughstart <- function(t, y) {
#  d <- data.frame(t = t, y = y)
#  z <- d[["y"]]
#  a <- min(z)
#  d$z <- a - z
#  b <- coef(lm(z ~ t, d))[1]
#  d$z <- log(d$z/b)
#  c <- -coef(lm(z ~ t, d[is.finite(d$z),]))[2]
#  parms <- as.numeric(c(a, b, c))
#  setNames(as.list(parms), c("a", "b", "c"))
#}

#start = roughstart(d$t, d$y)
