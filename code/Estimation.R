# Startup MAU Bass estimation 

source("Bass functions.R")
source("tg_base_functions.R")

library(GA) #genetic_algorithm_NLS

d <- read.csv("data.csv", header=T)

resultM <- matrix(0, 500, dim(d)[2]*2)

for (comi in 1:dim(d)[2]) {
  y <- as.numeric(d[,comi])
  y <- y[y > 0]
  x <- diff(y)
  t <- 1:length(y)

  esti <- BassGANLS(t=t, y=y, opt=1)
  p.esti <- esti$coefficients[,1]
  yhati <- BassNt(p.esti[1], p.esti[2], p.esti[3], t)

  resultM[1:(length(y)+10), ((comi-1)*2+1):((comi-1)*2+2)] <- 
          rbind(esti$coefficients[, c(1,4)], t(fitness(y, yhati ,3)), cbind(y, yhati))
  
}

write.csv(resultM, "results.csv")

