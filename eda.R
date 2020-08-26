#eda arroz
library(tidymodels)
library(tidyverse)
library(readxl)
library(ranger)
library(tune)
rice<-read_excel("data/rice-reduced.xlsx")

class(rice$Class)
rice$Class <-as.factor(rice$Class)
str(rice)

#necesito limpiar los nombres de las columnas
library(janitor)
rice2<-rice%>%
  clean_names()
#quiero remover variables correlacionadas, debo armar la receta
rice_corr <- rice2 %>% #creo un objeto a partir de iris
  initial_split(prop = 0) #75% de los datos
rice_corr
#elimino variables correlacionadas
rice_recipe2 <- training(rice_corr) %>%
  recipe(class~.) %>%
  step_corr(all_predictors(), threshold=0.7)%>%
  prep()

rice_after<-juice(rice_recipe2)
glimpse(rice_after)

library(GGally)
ggscatmat(rice_after, color = "class", alpha = 0.7)

rice_after %>% #datos de entrada
  ggpairs(aes(color = class),  alpha = 0.7)+ 
  theme_bw() #función ggpairs

ggsave("plots/ggally01.jpeg",  height=8, width=10, units="in", dpi=300)


library(tidymodels)

#agregar nueva variable a rice2


rice2id<-juice(pca_prep)%>%
  mutate(name = rownames(.)) %>% 
  select(name, everything())

pca_rec <- recipe(~., data = rice2id) %>%
  update_role(class, name, new_role = "id") %>%
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

library(tidytext)
  
  tidied_pca %>%
    filter(component %in% paste0("PC", 1:4)) %>%
    group_by(component) %>%
    top_n(8, abs(value)) %>%
    ungroup() %>%
    mutate(terms = reorder_within(terms, abs(value), component)) %>%
    ggplot(aes(abs(value), terms, fill = value > 0)) +
    geom_col() +
    facet_wrap(~component, scales = "free_y") +
    scale_y_reordered() +
    labs(
      x = "Absolute value of contribution",
      y = NULL, fill = "Positive?"
    )
  ggsave("plots/pca_components.jpeg",  height=8, width=10, units="in", dpi=300)
  
juice(pca_prep) %>%
  ggplot(aes(PC1, PC4)) +
  geom_point(aes(color = id), alpha = 0.7, size = 2) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)


pca_prep
ggsave("plots/pca01.jpeg",  height=8, width=10, units="in", dpi=300)

juice(pca_prep)

juice(pca_prep)%>%
  mutate(name = rownames(.)) %>% 
  select(name, everything())

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = name)) +
  geom_point(aes(color = class), alpha = 0.7, size = 2) +
  #geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
  labs(color = NULL)