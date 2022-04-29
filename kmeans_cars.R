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

# ustawiamy nazwy samochodów jako nazwy wierszy i usuwamy kolumne car_names
rownames(dt) <- dt$car_name
dt$car_name <- NULL

# standaryzujemy dane
dt <- scale(dt)
