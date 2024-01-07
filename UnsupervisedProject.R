###########################
###UNSUPERVISED LEARNING###
###########################
#https://www.kaggle.com/datasets/mssmartypants/water-quality

#Data download and data structure
data <- read.csv("C:/Users/39334/Desktop/Stat Learn/waterQuality1.csv", encoding="UTF-8", stringsAsFactors=FALSE)
str(data)
dim(data)
data$ammonia <- as.numeric(data$ammonia)
data <- data[complete.cases(data$ammonia), ]
data <- subset(data, data$ammonia >= 0 )
summary(data)

data <- data[,-21]
data_st <- scale(data)

#k-means
library(tidyverse)
set.seed(100)
km2 <- kmeans(data_st, 2, nstart= 20) 
str(km2)
km2

n_clusters <- 10
wss <- numeric(n_clusters)
set.seed(100)

for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(data_st, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed')

k <- 3
set.seed(100)
km.out <- kmeans(data_st, centers = k, nstart = 20)
km.out


#hierarchical clustering
library(dplyr)

dist_mat <- dist(data_st, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg, main = "Average Linkage")

cut_avg <- cutree(hclust_avg, k = 3)
plot(hclust_avg, main = "Average Linkage")
rect.hclust(hclust_avg , k = 3, border = 2:4)

data_df <- as.data.frame(data_st)  
data_avg <- mutate(data_df, cluster = cut_avg)
count(data_avg,cluster)

seg.medie<-function(data,groups){
  aggregate(data,list(groups),FUN=mean)
}

round(seg.medie(data_st,cut_avg),4)

hclust_cpl <- hclust(dist_mat, method = 'complete')
plot(hclust_cpl, main = "Complete Linkage")

cut_cpl <- cutree(hclust_cpl, k = 2)
plot(hclust_cpl, main = "Complete Linkage")
rect.hclust(hclust_cpl , k = 2, border = 2:6)
data_cpl <- mutate(data_df, cluster = cut_cpl)
count(data_cpl,cluster)
round(seg.medie(data_st,cut_cpl),4)






