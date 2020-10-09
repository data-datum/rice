#random forest con validacion cruzada

#divido los datos
set.seed(123)
rice_split<-initial_split(rice, strata=class)
rice_train<-training(rice_split)
rice_test<-testing(rice_split)

#validacion cruzada de arroz

p_folds <- vfold_cv(rice_train, strata = class)

rice_recipe<-recipe(class~., data=rice_split)%>%
  #step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
#vemos la salida
rice_recipe


#tuneo de numero de arboles---------------------------------------------------
tune_tree_spec <- rand_forest(
  trees=tune()
) %>%
  set_mode("classification")%>%
  set_engine("ranger")

#workflow 1
work_1 <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(tune_tree_spec)

set.seed(123)
work_1

doParallel::registerDoParallel()

set.seed(123)
tune_res <- tune_grid(
  work_1,
  resamples = trees_folds,
  grid = 20
)

tune_res


#tuneo de mtry y min_n -------------------------------------------------------------
set.seed(123)
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")
tune_spec

#armamos el workflow
tune_wf <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(tune_spec)
set.seed(123)
cv_folds <- vfold_cv(rice_train, strata = class)
tune_wf

#paralelizamos
doParallel::registerDoParallel()
set.seed(123)
tune_res <- tune_grid(
  tune_wf,
  resamples = cv_folds,
  grid = 20
)
tune_res

#ver como varian los valores en grandes intervalos
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "accuracy")

ggsave("tune-500.jpeg", height=8, width=10, units="in")


#con esto vemos como varian los valores
#seleccionamos para hacer el grid
#min_n entre 2 y 20 
#mtry entre 20 y 40

#armamos el grid con este intervalo

rf_grid <- grid_regular(
  mtry(range = c(20, 40)),
  min_n(range = c(2, 20)),
  levels = 5
)

rf_grid

#tuneo con la grilla
set.seed(123)
regular_res <- tune_grid(
  tune_wf,
  resamples = cv_folds,
  grid = rf_grid
)
#resultados
regular_res

#vamos a plotear
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "accuracy")

ggsave("accuracy-500.jpeg", height=8, width=10, units="in")


regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

ggsave("AUC-500.jpeg", height=8, width=10, units="in")


#elegimos el mejor modelo
set.seed(123)
best_acc <- select_best(regular_res, "accuracy")
final_rf <- finalize_model(
  tune_spec,
  best_acc
)
final_rf


#vamos a probar con los datos de testing
set.seed(123)
final_wf <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(final_rf)
final_res <- final_wf %>%
  last_fit(rice_split)
final_res %>%
  collect_metrics()


final_res %>%
  collect_predictions() %>%
  conf_mat(class, .pred_class)

#importancia de las variables
library(vip)
set.seed(123)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(class ~ .,
      data = juice(rice_recipe)) %>%
  vip(geom = "point")

ggsave("vip-500.jpeg", height=8, width=10, units="in")



