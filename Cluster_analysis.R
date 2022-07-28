##Data Loading
#getwd()
#setwd("source")

company <- read.csv("parameter.csv", header = T, sep = ",", row.names = 1)

#drop R2
drop <- c("Adjusted.R2", "X.t.")
company = company[, !(names(company) %in% drop)]
head(company)
dim(company)

##parameters
#mean scaled
company_scaled <- data.frame(scale(company))
t_star <- company_scaled$t.
t_star2 <- company_scaled$t..

#log scaled (except t*, t**)
log_scaled <- data.frame(log(company))

log_scaled$t. <- t_star
log_scaled$t.. <- t_star2


##Cluster analysis
#hierarchical clustering(dendrogram)
dist_eucl <- dist(log_scaled, method = "euclidean")

set.seed(1004)
hc_ward <- hclust(dist_eucl, method = "ward.D")
fviz_dend(hc_ward, k=4,
          k_colors = c("#FFA7A7", "#8C8C8C", "#6799FF", "#FFC000"))

groups1 <- cutree(hc_ward, k=4)
log_scaled[, "cluster"] <- groups1

#write.csv(log_scaled, file = "export_results(k=4).csv", row.names = TRUE)

plot(hc_ward, cex = 0.9)
rect.hclust(hc_ward, k = 4, border = 2:6)

fviz_dend(hc_ward, k=4, color_labels_by_k = TRUE)

##Plot
#library(factoextra)
fviz_cluster(list(data = log_scaled, cluster = log_scaled$cluster))

fviz_cluster(list(data = company_11, cluster = company_11$cluster), ggtheme = theme_minimal()) +
  ggtitle("Cluster analysis - parameter: p, q, q/p, t*") +
  scale_colour_manual(values = c("#FFA7A7", "#8C8C8C", "#6799FF", "#FFC000")) +
  scale_fill_manual(values = c("#FFA7A7", "#8C8C8C", "#6799FF", "#FFC000"))


##Statistics test
#ANOVA
data <- read.csv("export_results(k=4).csv", header = T, sep = ",", row.names = 1)
test_p <- aov(p~cluster, data = data)
test_q <- aov(q~cluster, data = data)
test_qp <- aov(q.p~cluster, data = data)
test_t1 <- aov(t.~cluster, data = data)

summary(test_p)
summary(test_q)
summary(test_qp)
summary(test_t1)

aggregate(test_p~cluster, data = data,mean)


##Post-Hoc test
#library("DescTools")
PostHocTest(test_p, method = 'hsd')
PostHocTest(test_q, method = 'hsd')
PostHocTest(test_qp, method = 'hsd')
PostHocTest(test_t1, method = 'hsd')


#remove outlier
company_remove <- c("프렌즈큐브", "머지홀딩스", "밸류업시스템즈","드림모션", "핏펫", "케이크")
company = company[!(row.names(company) %in% company_remove),]


#curve plot example
##curve plot
dpm <- read.csv("plot.csv", header=T)
date4 <- as.Date.factor(dpm$date)


ggplot(dpm, aes(x=date4, y=y_hat, group = company))+
  geom_line(aes(colour = company), size = 2)+
  geom_point(aes(x=date4, y=y, colour = company))+
  labs(x="time", y="MAU", title = "Growth pattern of start-up(one of each cluster)") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual(values = palette_5)

palette_5 = c('[C1] 컬리' = '#FFA7A7',  '[C2] 카카오뱅크' = '#8C8C8C', '[C3] 당근마켓' = '#6799FF', '[C4] 라프텔' = '#FFC000')


