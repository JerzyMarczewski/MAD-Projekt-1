install.packages("e1071")
install.packages("factoextra")
install.packages("cluster")
install.packages("tidyr")
install.packages("ggplot2")
library(tidyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(e1071) 
library(RCurl)
library(kohonen)

sekwencja <- seq(1, 298, 2)

# pobieranie danych
cars_gh <- getURL("https://raw.githubusercontent.com/JerzyMarczewski/MAD-Projekt-1/main/cars_multi.csv")
dt <- read.csv(text = cars_gh, header = T, sep = ',', dec = '.')

# redukcja ilosci wierszy w celu lepszej przejrzystości wyników
dt <- dt[sekwencja,]

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


################################################################################
# metoda łokciowa
fviz_nbclust(dt, kmeans, method = "wss")

# metoda silhouette
windows()
fviz_nbclust(dt, kmeans, method = "silhouette")


clusters <- 3

# k-srednich
km <- kmeans(dt, centers = clusters)
windows()
fviz_cluster(km, data = dt, repel = T)


################################################################################
# metoda warda
odleglosci <- dist(dt)
grupy <- agnes(odleglosci, method = "ward")
windows()
fviz_dend(grupy, k = clusters, rect = TRUE, main = "Metoda Ward")

# metoda complete
grupy_complete <- agnes(odleglosci, method = "complete")
fviz_dend(grupy_complete, k = clusters, rect = TRUE, main = "Metoda complete")

# metoda single
grupy_single <- agnes(odleglosci, method = "single")
fviz_dend(grupy_single, k = clusters, rect = TRUE, main = "Metoda single")

# metoda average
grupy_average <- agnes(odleglosci, method = "average")
fviz_dend(grupy_average, k = clusters, rect = TRUE, main = "Metoda average")


################################################################################
# metoda Self-organising maps
cars_SOM <- as.matrix(dt)
cars_grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
cars_SOM_model <- som(cars_SOM, grid = cars_grid)

windows()
# Plot our results
# Plot type 1: counts
plot(cars_SOM_model, type = "counts")
# bardziej czerwone maja mniej obserwacji, żółty wiecej obserwacji, szary brak obserwacji
# zmniejszyc liczbe gridów, bo wystepuja szare okregi


# Plot type 2: heatmap
# Dla sepal.width
plot(cars_SOM_model, type = "property",
     property = getCodes(cars_SOM_model)[,4],
     main = colnames(dt)[4])


# Plot type 3: fan diagram
plot(cars_SOM_model, type = "codes")


# Plot type 4: trainig progress
plot(cars_SOM_model, type = "changes")


# Plot type 5: neighbour distance
plot(cars_SOM_model, type = "dist.neighbours")


# boxploty
windows()
boxplot(dt)


# histogramy
# no scaling dataset

dt %>%
  ggplot(aes(x = mpg)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = cylinders)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = displacement)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = horsepower)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = weight)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = acceleration)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = model)) +
  geom_histogram()

dt %>%
  ggplot(aes(x = origin)) +
  geom_histogram()
