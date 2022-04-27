data <- read.csv("C:\\Users\\wojte\\Desktop\\MAD-Projekt-1\\Nakłady_inwestycyjne_roczne.csv", header = T, sep = ";")


boxplot(data[2:12])
boxplot(scale(data[2:12]))

install.packages("tidyr")
install.packages("ggplot2")
library(tidyr)
library(ggplot2)

install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)

data_stand <- scale(data[2:12])
fviz_nbclust(data_stand, kmeans, method = "wss")

k_mean <- kmeans(data_stand, centers = 2)
fviz_cluster(k_mean, data = data_stand, repel = T)
