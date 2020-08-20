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
rice_split<-initial_split(rice, strata=Class)
rice_train<-training(rice_split)
rice_test<-testing(rice_split)


rice_recipe<-recipe(Class~., data=rice_split)%>%
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