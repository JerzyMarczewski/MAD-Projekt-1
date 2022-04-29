install.packages("e1071")
install.packages("factiextra")
install.packages("cluster")
library(factoextra)
library(cluster)
library(e1071) 


# UWAGA W PLIKU cars_mutli.csv ręcznie zamieniłem znak '?' na ''

dt <- read.csv(
  "C:\\Users\\Jerzy\\Documents\\nauka\\MAD\\cars_multi.csv", 
  header = T, 
  sep = ',',
  dec = '.')

# usuwamy puste wiersze 
dt <- na.omit(dt)

# pozostawiamy tylko wiersze z unikalna wartoscia w kolumnie car_name
dt <- dt[!duplicated(dt$car_name),]

# ustawiamy nazwy samochodów jako nazwy wierszy i usuwamy kolumne car_names i ID
rownames(dt) <- dt$car_name
dt$car_name <- NULL
dt$ID <- NULL

# srednia | mediana  | min | max | odchylenie standardowe | skośność
sapply(dt, mean)
sapply(dt, median)
sapply(dt, min)
sapply(dt, max)
sapply(dt, sd)
sapply(dt, skewness)

# standaryzujemy dane
dt <- scale(dt)

# metoda łokciowa
fviz_nbclust(dt, kmeans, method = "wss")

# metoda silhouette
fviz_nbclust(dt, kmeans, method = "silhouette")

# k-srednich
options(ggrepel.max.overlaps = 0)
km <- kmeans(dt, centers = 3)
fviz_cluster(km, data = dt, repel = T)
