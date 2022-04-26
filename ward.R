library(archivist)
install.packages('archivist')
auta <- archivist::aread("pbiecek/Przewodnik/arepo/bf2846de03bc8434d234b08fd2e31694")
auta$nazwa <- rownames(auta)
auta$Cena_norm <- scale(sqrt(auta$Cena))
auta$KM_norm <- scale(sqrt(auta$KM))
odleglosci <- dist(auta[,c("Cena_norm", "KM_norm")], method = "manhattan")
as.matrix(odleglosci)[1:5,1:5]

library(cluster)
grupy <- agnes(odleglosci, method = "ward")

library(factoextra)
fviz_dend(grupy, k = 4, rect = TRUE, main = "Metoda Ward")

library(RColorBrewer)
install.packages('ape')
library(ape)
cols <- brewer.pal(4,"Set1")
hc <- as.phylo(as.hclust(grupy))

par(mar=c(1,1,2,1), xpd=NA)
plot(hc, type = "fan", cex = 0.8,
     tip.color = cols[auta$grupa])


