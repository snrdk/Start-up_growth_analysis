setwd("\\path")

anova <- read.csv("file.csv", header = T, sep = ",", row.names = NULL)

p <- aov(p~column, data = anova)
q <- aov(q~column, data = anova)
t1 <- aov(t.~column, data = anova)

summary(p)
summary(q)
summary(t1)

#compaison by mean
aggregate(p~column, data = anova, mean)
aggregate(q~column, data = anova, mean)
aggregate(t.~column, data = anova, mean)


##Post-Hoc test
#library("DescTools")
PostHocTest(p, method = 'hsd')
PostHocTest(q, method = 'hsd')
PostHocTest(t1, method = 'hsd')


x = anova$column
y = anova$cluster

x1; y1

result = data.frame(Level = x, Pass = y)
dim(result)

#cross-table
table(result1)

#install.packages("gmodels")
#library(gmodels)
CrossTable(x = x1, y= y1)
CrossTable(x1, y1, chisq = TRUE)