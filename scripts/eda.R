#analisis exploratorio
#PCA con tidymodels 

library(tidymodels)
library(tidyverse)
library(readxl)
library(ranger)
library(tune)
#rice<-read_excel("data/rice.xlsx")

class(rice$Class)
rice$Class <-as.factor(rice$Class)
str(rice)

library(janitor)
rice<-rice%>%
  clean_names()

#GGally
rice %>% #datos de entrada
  ggpairs(aes(color = class))+ theme_bw() #función ggpairs

  
#PCA

pca_rec <- recipe(~., data = rice) %>%
  update_role(class, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

pca_prep

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

#agrego rownames
pca_row <- juice(pca_prep)%>%
  mutate(name = rownames(.)) %>% 
  select(name, everything())


pca <- pca_row %>%
  ggplot(aes(PC1, PC2, label=name)) +
  geom_point(aes(color = class), alpha = 0.7, size = 2) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)


library(plotly)
ggplotly(pca)


pca_prep
ggsave("plots/pca01.jpeg",  height=8, width=10, units="in", dpi=300)


#umap
library(embed)
umap_rec <- recipe(~., data = rice) %>%
  update_role(class, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

juice(umap_prep) %>%
  ggplot(aes(umap_1, umap_2, label = class)) +
  geom_point(aes(color = class), alpha = 0.7, size = 2) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)


ggsave("plots/umap_01.jpeg",  height=8, width=10, units="in", dpi=300)
