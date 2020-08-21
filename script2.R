---------------------#modelo con todas las variables -------------------------------
#cargo librerias
library(tidymodels)
library(tidyverse)
library(readxl)
library(ranger)
library(tune)
rice<-read_excel("data/rice-reduced.xlsx")
    
#la columna Class esta codificada como caracter y necesito que sea FACTOR
class(rice$Class)
rice$Class <-as.factor(rice$Class)
str(rice)
    
#necesito limpiar los nombres de las columnas
library(janitor)
rice2<-rice%>%
  clean_names()
    
#divido los datos
set.seed(123)
rice_split<-initial_split(rice2, strata=class)
rice_train<-training(rice_split)
rice_test<-testing(rice_split)
    
    
rice_recipe<-recipe(class~., data=rice_split)%>%
  #step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
#vemos la salida
rice_recipe
    
rice_training <- juice(rice_recipe)
    
rice_testing <- rice_recipe %>%
  bake(testing(rice_split))
  head(rice_testing)
    
    
#tuning de random forest
rf_tune<-rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
)%>%
  set_mode("classification")%>%
  set_engine("ranger")
    
rf_tune
#workflow para modelos
    
rice_wf <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(rf_tune)
    
#hyperparameter tunning
set.seed(234)
trees_folds <- vfold_cv(rice_train)
#paralelizar los cálculos
doParallel::registerDoParallel()
    
set.seed(345)
tune_res <- tune_grid(
  rice_wf,
  resamples = trees_folds,
  grid = 20
)
    
tune_res
    
    
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
              values_to = "value",
              names_to = "parameter"
) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")
    
ggsave("mtry-min-n.jpeg", height=8, width=10, units="in")

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)
    
rf_grid
    
      
#rf_grid %>%
#  collect_metrics()
    
set.seed(456)
regular_res <- tune_grid(
  rice_wf,
  resamples = trees_folds,
  grid = rf_grid
)
    
regular_res
    
    
    
regular_res %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    mutate(min_n = factor(min_n)) %>%
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line(alpha = 0.5, size = 1.5) +
    geom_point() +
    labs(y = "AUC")
    
ggsave("mtry-optimization.jpeg", height=8, width=10, units="in")
    
best_auc <- select_best(regular_res, "roc_auc")
    
#elegimos el mejor modelo 
final_rf <- finalize_model(
  rf_tune,
  best_auc
)
    
final_rf
    
#finalmente el modelo 
final_wf <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(final_rf)
    
final_res <- final_wf %>%
  last_fit(rice_split)
#metricas
final_res %>%
  collect_metrics()
    
    
