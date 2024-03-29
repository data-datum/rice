#xgboost con CV = 3

library(tidymodels)

set.seed(123)
rice_split<-initial_split(rice, strata=class)
rice_train<-training(rice_split)
rice_test<-testing(rice_split)

rice_split

#specifications of the model
set.seed(123)
xgb_spec <- boost_tree(
  trees = tune(), 
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
  trees(),
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
vb_folds_3 <- vfold_cv(rice_train, strata = class, v = 3)
vb_folds_3


doParallel::registerDoParallel()

set.seed(123)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds_3,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

glimpse(xgb_res$.metrics)


#vemos las metricas----------------------------------------------------------
collect_metrics(xgb_res)
#xgboost plot
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "accuracy")

ggsave("plots2/boost-tune-cv-3.jpeg", height=8, width=10, units="in", dpi=300)
ggsave("plots2/boost-tune-cv-3-.tiff", height=8, width=10, units="in", dpi=300)



#ROC CURVE
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
  labs(x = NULL, y = "roc_auc")

ggsave("plots2/boost-tune-rocauc-cv-3.jpeg", height=8, width=10, units="in", dpi=300)
ggsave("plots2/boost-tune-rocauc-cv-3.tiff", height=8, width=10, units="in", dpi=300)


#elegimos el mejor modelo------------------------------------------------------ 
show_best(xgb_res, "accuracy")

best_auc <- select_best(xgb_res, "accuracy")
best_auc

final_xgb <- finalize_workflow(
  xgb_wf,
  best_auc
)

final_xgb

#importancia de las variables-----------------------------------------------
  library(vip)
  set.seed(123)
  final_xgb %>%
    fit(data = rice_train) %>%
    pull_workflow_fit() %>%
    vip(geom = "col", num_features = 15, color='gray70', fill='gray70') +
    labs(title="XGBoost importance variables", x='variables',y='importance')+
    theme_light()+
    theme(axis.title.x=element_text(size=16), axis.title.y=element_text(size=16),
          axis.text.x=element_text(size=12),axis.text.y=element_text(size=14))
  
  ggsave("plots2/boost-vip-3.jpeg", height=8, width=10, units="in", dpi=300)
  ggsave("plots2/boost-vip-3.tiff", height=8, width=10, units="in", dpi=300)


xgb<-final_xgb %>%
  fit(data = rice_train) %>%
  pull_workflow_fit()

variables_imp<-vi(xgb, sort=TRUE)
write.csv(variables_imp, file="imp_variables-cv3.csv")


#matriz de confusion-------------------------------------------------
set.seed(123)
final_res <- last_fit(final_xgb, rice_split)

final_res %>%
  collect_predictions() %>%
  conf_mat(class, .pred_class)%>%
  autoplot(type = "heatmap")

ggsave("plots2/conf-heatmap-cv.3.jpeg", height=8, width=10, units="in", dpi=300)
ggsave("plots2/conf-heatmap-cv-3.tiff", height=8, width=10, units="in", dpi=300)

collect_metrics(final_res)


# curvas roc---------------------------------------------------------
set.seed(123)
final_res%>%
  collect_predictions() %>% 
  roc_curve(class, .pred_10_adulteration:.pred_pure_variety)%>%
  autoplot()

ggsave("roc-curves-3.jpeg", height=8, width=10, units="in", dpi=300)
ggsave("roc-curves-3.tiff", height=8, width=10, units="in", dpi=300)

set.seed(123)
final_res %>%
  collect_predictions()%>%
  conf_mat(class, .pred_class) %>%
  summary() %>%
  select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "precision", "recall", "f_meas", "roc_auc")) 


#training error-------------------------------------------------------
