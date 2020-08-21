#script 3
#trabajamos con las variables eliminadas

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
  step_corr(all_predictors(), threshold=0.7) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
#vemos la salida
rice_recipe

recipe_training <- juice(rice_recipe)
glimpse(recipe_training)
#quedaron solo 12 variables y 119 filas

set.seed(123)
rice_bootstrap <- bootstraps(rice_train, times=5)
rice_bootstrap

#vamos a hacer random forest sin tunning
rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec

rice_wf <- workflow() %>%
  add_formula(class ~ .)

rice_wf

rf_rs <- rice_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = rice_bootstrap,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs


collect_metrics(rf_rs)

#matriz de confusion de datos de training
rf_rs %>%
  conf_mat_resampled()

#no olvidemos de preprocesar los datos de testing TAMBIEN
rice_testing <- rice_recipe %>%
  bake(testing(rice_split))

RF_final <- rice_wf %>%
  add_model(rf_spec) %>%
  last_fit(rice_split)

RF_final
#vemos las metricas del modelo final

collect_metrics(RF_final)

collect_predictions(RF_final) %>%
  conf_mat(class, .pred_class)



#graficamos las curvas ROC del entrenamiento
rf_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(class, .pred_Adulterated:.pred_Japonica) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = TRUE, alpha = 0.6, size = 1.2) +
  coord_equal()

ggsave("curvas-roc-bootstraping.jpeg", height=8, width=10, units="in")