mtcars
boxplot(mtcars)

# standaryzacja
boxplot(scale(mtcars))

?mtcars

install.packages("tidyr")
install.packages("ggplot2")
library(tidyr)
library(ggplot2)

cars <- mtcars
# histogram

cars %>%
  ggplot(aes(x = disp)) +
  geom_histogram()

cars_1 <- 
cars %>%
  pivot_longer(cols = colnames(cars)) %>%
  
  ggplot(aes(x = value)) +
  geom_histogram(aes(fill = name)) +
  facet_wrap(~name, scales = "free")

# boxplot

cars %>%
  pivot_longer(cols = colnames(cars)) %>%
  
  ggplot(aes(y = value)) +
  geom_boxplot(aes(fill = name)) +
  facet_wrap(~name, scales = "free")

# violin 

cars %>%
  pivot_longer(cols = colnames(cars)) %>%
  
  ggplot(aes(y = value)) +
  geom_violin(aes(x = name, fill = name)) +
  facet_wrap(~name, scales = "free")



### analiza skupien
#wykres osuwiska, metoda lokciowa

install.packages("cluster")
install.packages("factoextra")
library(cluster)
library(factoextra)


#standaryzacja
cars_s <- scale(cars)

fviz_nbclust(cars_s, kmeans, method = "wss")

#liczba klastrów

#klasteryzacja
km1 <- kmeans(cars_s, centers = 4)
fviz_cluster(km1, data = cars_s, repel = T)
# lepiej dla 4
km2 <- kmeans(cars_s, centers = 3)
fviz_cluster(km2, data = cars_s)





