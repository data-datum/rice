#pca para detectar outliers
library("FactoMineR")
library("factoextra")
rice2solo_num<- rice2 %>%
  select(-class)
res.pca <- PCA(rice2solo_num, graph = TRUE)

library("factoextra")
eig.val <- get_eigenvalue(res.pca)
eig.val

jpeg("pca_300.jpeg",  height=8, width=10,
     units = "in", res = 300) #para guardarlo en tiff
fviz_pca_ind(res.pca, 
             pointshape = 21,
             repel = TRUE # Avoid text overlapping (slow if many points)
)

dev.off()
