-------#script arroz michael------------------------------------
library(tidyverse)
library(readxl)
rice<-read_excel("data/rice-reduced.xlsx")
library(corrplot) #librería corrrplot para graficar


---------------#grafico de correlacion de variables-----------
rice_cor<-rice %>% #ingreso de datos
  select(-Class) #elimino variable Species
M <- cor(rice_cor) #calculo matriz de correlación 
corrplot(M, order="hclust", col=c("black", "white"),
         bg="lightblue") #grafico
tiff("corrplot_300.tif", width = 12, height = 12,
     units = "cm", res = 300) #para guardarlo en tiff
corrplot(M, order="hclust", tl.col="black", 
         #tl.srt=45,  
         tl.cex = 0.4) #grafico

dev.off()

jpeg("corrplot_400.jpg", width = 12, height = 12,
     units = "cm", res = 400)
corrplot(M, order="hclust", tl.col="black", 
         #tl.srt=45,  
         tl.cex = 0.4) #grafico

dev.off()

