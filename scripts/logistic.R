#logistic regression

library(tidymodels)

set.seed(123)
rice_split<-initial_split(rice, strata=class)
rice_train<-training(rice_split)
rice_test<-testing(rice_split)


set.seed(123)
rice_boot <- bootstraps(rice_train)
rice_boot
#especifico el modelo
glm_spec <- logistic_reg() %>%
  set_engine("glm")
glm_spec

#workflow
tune_wf <- workflow() %>%
  add_recipe(rice_recipe) %>%
  add_model(glm_spec)
set.seed(123)
cv_folds <- vfold_cv(rice_train, strata = class)
tune_wf

#resampling
glm_rs <- tune_wf %>%
  #add_model(glm_spec) %>%
  fit_resamples(
    resamples = rice_boot,
    control = control_resamples(save_pred = TRUE)
  )

glm_rs


collect_metrics(glm_rs)





