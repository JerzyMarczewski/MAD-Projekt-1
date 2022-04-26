data <- read.csv("C:\\Users\\wojte\\Desktop\\Nakłady_inwestycyjne_roczne.csv", header = T, sep = ";")

library(cluster)

library(factoextra)

rownames(data) <- c("budynki i budowle", "maszyny urządzenia techniczne i narzędzia", "środki transportu", "przemysł", "górnictwo i wydobywanie", "przetwórstwo przemysłowe", "wytwarzanie i zaopatrywanie w energię elektryczną gaz parę wodną i gorącą wodę","dostawa wody gospodarowanie ściekami i odpadami rekultywacja","budownictwo","handel naprawa pojazdów samochodowych","transport i gospodarka magazynowa","obsługa rynku nieruchomości")
data$rok_2010_norm <- scale(sqrt(data$rok_2010))
data$rok_2011_norm <- scale(sqrt(data$rok_2011))
data$rok_2012_norm <- scale(sqrt(data$rok_2012))
data$rok_2013_norm <- scale(sqrt(data$rok_2013))
data$rok_2014_norm <- scale(sqrt(data$rok_2014))
data$rok_2015_norm <- scale(sqrt(data$rok_2015))
data$rok_2016_norm <- scale(sqrt(data$rok_2016))
data$rok_2017_norm <- scale(sqrt(data$rok_2017))
data$rok_2018_norm <- scale(sqrt(data$rok_2018))
data$rok_2019_norm <- scale(sqrt(data$rok_2019))
data$rok_2020_norm <- scale(sqrt(data$rok_2020))


odleglosci <- dist(data[,c("rok_2010_norm", "rok_2011_norm", "rok_2012_norm", "rok_2013_norm", "rok_2014_norm", "rok_2015_norm","rok_2016_norm","rok_2017_norm","rok_2018_norm","rok_2019_norm", "rok_2020_norm")])
as.matrix(odleglosci)[1:12,1:12]
grupy <- agnes(odleglosci, method = "ward")
guides(scale = "none")
fviz_dend(grupy, k = 4, rect = TRUE, main = "Metoda Ward")
grupy
data


cols <- brewer.pal(4,"Set1")
hc <- as.phylo(as.hclust(grupy))

par(mar=c(1,1,2,1), xpd=NA)
plot(hc, type = "fan", cex = 0.8,
     tip.color = cols[data$grupa])

