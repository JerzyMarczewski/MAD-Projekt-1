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

# standaryzujemy dane
dt <- scale(dt)

# metoda warda
odleglosci <- dist(dt)
grupy <- agnes(odleglosci, method = "ward")
windows()
fviz_dend(grupy, k = 4, rect = TRUE, main = "Metoda Ward")

# metoda complete
grupy_complete <- agnes(odleglosci, method = "complete")
fviz_dend(grupy_complete, k = 4, rect = TRUE, main = "Metoda complete")

# metoda single
grupy_single <- agnes(odleglosci, method = "single")
fviz_dend(grupy_single, k = 4, rect = TRUE, main = "Metoda single")

# metoda average
grupy_average <- agnes(odleglosci, method = "average")
fviz_dend(grupy_average, k = 4, rect = TRUE, main = "Metoda average")
