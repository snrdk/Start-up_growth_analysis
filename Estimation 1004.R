getwd()
setwd("C:\\Users\\user\\Desktop\\References\\박사\\source")

# Startup MAU Bass estimation 

source("Bass functions.R")
source("tg_base_functions.R")

library(GA)

d <- read.csv("rawdata_ex.csv", header=T)

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

write.csv(resultM, "result_ex.csv")


##Plot############################################################################################
library(ggplot2)
library(dplyr)
library(reshape)

#fashion
dp1 <- read.csv("plot1.csv", header=T)
date <- as.Date.factor(dp1$date)

ggplot(dp1, aes(x=date, y=y_hat, group = company))+
  geom_line(aes(colour = company))+
  geom_point(aes(x=date, y=y, colour = company))+
  labs(x="time", y="estimated MAU", title = "Estimated MAU of fashion industry start-up")+

#real-estate
dp2 <- read.csv("plot3.csv", header=T)
date2 <- as.Date.factor(dp2$date)

ggplot(dp2, aes(x=date2, y=y_hat, group = company))+
  geom_line(aes(colour = company))+
  geom_point(aes(x=date2, y=y, colour = company))+
  labs(x="time", y="estimated MAU", title = "Estimated MAU of real estate industry start-up")





dp$date <- as.Date.factor(dp$date)

View(dp)
date <- dp["date"]


plot(x = date, y = dp$y1, pch = 1, xlab = "와디즈플랫폼", ylab = "monthly active users")
lines(x = date, y = dp$y_hat1, type = "l")
abline(v = date[27.22], col = 'red')

plot(x = date, y = dp$y2, pch = 1, xlab = "호갱노노", ylab = "monthly active users")
lines(x = date, y = dp$y_hat2, type = "l")
abline(v = date[22.98], col = 'red')

plot(x = date, y = dp$y3, pch = 1, xlab = "왓챠", ylab = "monthly active users")
lines(x = date, y = dp$y_hat3, type = "l")
abline(v = date[129.21], col = 'red')

plot(x = date, y = dp$y4, pch = 1, xlab = "에이블리", ylab = "monthly active users")
lines(x = date, y = dp$y_hat4, type ="l")
abline(v = date[33.41], col = 'red')

plot(x = date, y = dp$y5, pch = 1, xlab = "당근마켓", ylab = "monthly active users")
lines(x = date, dp$y_hat5, type ="l" )
abline(v = date[39.16], col = 'red')

plot(x = date, y = dp$y5, pch = 1, xlab = "당근마켓", ylab = "monthly active users")
lines(x = date, dp$y_hat5, type ="l" )
abline(v = date[39.16], col = 'red')






###parameter function
date <- as.Date.factor(dp$date)

cc <- function(p, q){
  t2 <- {(-1)/(p+q)}*ln((2+sqrt(3))*(p/q))
}

ch <- function(p, q){
  t1 <- {1/(p+q)}*ln(q/p)
}

st <- function(m, p, q){
  st1 <- {m*(p+q)^2}/(4*q)
}


###parameter

##library(SciViews)

#t**
T1 = cc(0.00111041854980748, 0.12365772434534)
T2 = cc(0.00230955532625581,0.107496681)
T3 = cc(0.0000520934358735394, 0.0414493450384395)
T4 = cc(0.000106528662994355, 0.18352072188471)
T5 = cc(0.000161192439806645, 0.138801030586186)

T6 = cc(0.002309555, 0.107496681) #호갱노노
T7 = cc(0.00933745754484597, 0.0218914122653782) #두꺼비세상
T8 = cc(0.000506148878941474, 0.00633151006218716) #밸류업시스템즈
T9 = cc(0.000801624438875934, 0.0553682663446716) #아파트너
T10 = cc(0.00996404089000432, 0.0629457007589881) #집토스
T11 = cc(0.000630414769412355, 0.17665496613548) #디스코

T12 = cc(0.00621300619564804, 0.0745982105398491) #무신사
T13 = cc(0.000106531739649568,0.18351989033209) #에이블리
T14 = cc(0.020271236, 0.007158218) #난다
T15 = cc(0.0372584470291438, 0.0132230629314466) #크로키닷컴
T16 = cc(0.000186960097359664, 0.0897139362196786) #엔코드
T17 = cc(0.00295760901691044,0.0835251409412259) #브랜디


#t*
h1 = ch(0.00111041854980748, 0.12365772434534)
h2 = ch(0.00230955532625581,0.107496681)
h3 = ch(0.0000520934358735394, 0.0414493450384395)
h4 = ch(0.000106528662994355, 0.18352072188471)
h5 = ch(0.000161192439806645, 0.138801030586186)

h6 = ch(0.002309555, 0.107496681) #호갱노노
h7 = ch(0.00933745754484597, 0.0218914122653782) #두꺼비세상
h8 = ch(0.000506148878941474, 0.00633151006218716) #밸류업시스템즈
h9 = ch(0.000801624438875934, 0.0553682663446716) #아파트너
h10 = ch(0.00996404089000432, 0.0629457007589881) #집토스
h11= ch(0.000630414769412355, 0.17665496613548) #디스코

h12 = ch(0.00621300619564804, 0.0745982105398491) #무신사
h13 = ch(0.000106531739649568,0.18351989033209) #에이블리
h14 = ch(0.020271236, 0.007158218) #난다
h15 = ch(0.0372584470291438, 0.0132230629314466) #크로키닷컴
h16 = ch(0.000186960097359664, 0.0897139362196786) #엔코드
h17 = ch(0.00295760901691044,0.0835251409412259) #브랜디

#s(t*)
s6 = st(2718097, 0.002309555, 0.107496681) #호갱노노
s7 = st(353311.354130456, 0.00933745754484597, 0.0218914122653782) #두꺼비세상
s8 = st(6817970.56925776, 0.000506148878941474, 0.00633151006218716) #밸류업시스템즈
s9 = st(1815815.59324384, 0.000801624438875934, 0.0553682663446716) #아파트너
s10 = st(69346, 0.00996404089000432, 0.0629457007589881) #집토스
s11 = st(227581, 0.000630414769412355, 0.17665496613548) #디스코

s12 = st(5713862.80558079, 0.00621300619564804, 0.0745982105398491) #무신사
s13 = st(3494509, 0.000106531739649568,0.18351989033209) #에이블리
s14 = st(37485, 0.020271236, 0.007158218) #난다
s15 = st(3531038, 0.0372584470291438, 0.0132230629314466) #크로키닷컴
s16 = st(145202.562650697, 0.000186960097359664, 0.0897139362196786) #엔코드
s17 = st(1739883.51077676, 0.00295760901691044,0.0835251409412259) #브랜디

