# Load package
library(kohonen)
library(cluster)
library(factoextra)

dt <- read.csv(
  "C:\\Users\\wojte\\Desktop\\cars_multi.csv", 
  header = T, 
  sep = ',',
  dec = '.')

# usuwamy puste wiersze 
dt <- na.omit(dt)

# pozostawiamy tylko wiersze z unikalna wartoscia w kolumnie car_name
dt <- dt[!duplicated(dt$car_name),]

# ustawiamy nazwy samochodów jako nazwy wierszy i usuwamy kolumne car_names
rownames(dt) <- dt$car_name
dt$car_name <- NULL
dt$ID <- NULL
dt$origin <- NULL

str(dt) # 301 obserwacje, 7 zmiennych

# standaryzujemy dane
dt <- scale(dt)

cars_SOM <- as.matrix(dt)
cars_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
set.seed(100)
cars_SOM_model <- som(cars_SOM, grid = cars_grid)


# Plot our results
# Plot type 1: counts
plot(cars_SOM_model, type = "counts")
# bardziej czerwone maja mniej obserwacji, żółty wiecej obserwacji, szary brak obserwacji
# zmniejszyc liczbe gridów, bo wystepuja szare okregi


# Plot type 2: heatmap
# Dla sepal.width
plot(cars_SOM_model, type = "property",
     property = getCodes(iris_SOM_model)[,2],
     main = colnames(dt)[2])


# Plot type 3: fan diagram
plot(cars_SOM_model, type = "codes")


# Plot type 4: trainig progress
plot(cars_SOM_model, type = "changes")


# Plot type 5: neighbour distance
plot(cars_SOM_model, type = "dist.neighbours")
