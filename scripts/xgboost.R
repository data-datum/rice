#tuning xgboost

  library(tidymodels)
  
  set.seed(123)
  rice_split<-initial_split(rice, strata=class)
  rice_train<-training(rice_split)
  rice_test<-testing(rice_split)
  
  #specifications of the model
  set.seed(123)
  xgb_spec <- boost_tree(
    trees = 500, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune(),                         ## step size
  ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  xgb_spec
  
  set.seed(123)
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), rice_train),
    learn_rate(),
    size = 30
  )
  
  xgb_grid
  
  #workflow
  xgb_wf <- workflow() %>%
    add_formula(class ~ .) %>%
    add_model(xgb_spec)
  
  xgb_wf
  
  #validacion cruzada
  set.seed(123)
  vb_folds <- vfold_cv(rice_train, strata = class)
  vb_folds
  
  
  doParallel::registerDoParallel()
  
  set.seed(123)
  xgb_res <- tune_grid(
    xgb_wf,
    resamples = vb_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
  )
  
  xgb_res
      
  #vemos las metricas
  collect_metrics(xgb_res)
  #xgboost plot
  xgb_res %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    select(mean, mtry:sample_size) %>%
    pivot_longer(mtry:sample_size,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "accuracy")
  
  ggsave("boost-500.jpeg", height=8, width=10, units="in")
  
  
  show_best(xgb_res, "roc_auc")
  
  best_auc <- select_best(xgb_res, "roc_auc")
  best_auc
  
  final_xgb <- finalize_workflow(
    xgb_wf,
    best_auc
  )
  
  final_xgb
  
  #importancia de las variables
  library(vip)
  set.seed(123)
  final_xgb %>%
    fit(data = rice_train) %>%
    pull_workflow_fit() %>%
    vip(geom = "point") +
    labs(title="XGboost importance variables")
  
  ggsave("boost-vip-500.jpeg", height=8, width=10, units="in")
  
  final_res <- last_fit(final_xgb, rice_split)
  
  #matriz de confusion
  final_res %>%
    collect_predictions() %>%
    conf_mat(class, .pred_class)
  
  collect_metrics(final_res)


final_res %>%
  collect_predictions() %>%
  roc_curve(class, .pred_10_adulteration:.pred_pure_variety) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

