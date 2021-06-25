# ------ Load Library -----
library("xgboost")
library("rBayesianOptimization")

TRAIN_DT <- as.Date("2021-01-01")

# ----- Register Board -----
pins::board_register_local(name = "Portfolio Management")

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data", "Portfolio Management") %>% 
  as.data.frame() %>% 
  
  # create factor TARGET
  dplyr::mutate(
    target = as.factor(dplyr::if_else(target_10w > 0, "Over", "Under"))
  ) %>% 
  as.data.frame() %>% 
  
  # - split train/test
  dplyr::mutate(
    sample_type = dplyr::if_else(dt < TRAIN_DT, "Train", "Test")
  ) %>% 
  
  # - Remove columns
  dplyr::select(-ticker, 
                -open, -high, -low, -close, -p_adjusted, -volume) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  as.data.frame()

train_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Train") %>% 
  dplyr::select(-sample_type, -dt) %>% 
  as.data.frame()

test_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Test") %>% 
  dplyr::select(-sample_type, -dt) %>% 
  as.data.frame()

# ----- prepare XGBOOST data structure -----
dtrain <- xgb.DMatrix(train_df %>% 
                        dplyr::select(-target_10w, -target) %>% 
                        as.matrix(), 
                      label = train_df$target_10w)
dval <- xgb.DMatrix(test_df %>% 
                      dplyr::select(-target_10w, -target) %>% 
                      as.matrix(), 
                    label = test_df$target_10w)

# ----- Create KFold Cross Validation
cv_folds <- KFold(target = train_df$target_10w, nfolds = 20, stratified = T)

# ----- Develop Cross Validated Xgboost Model -----
xgb_cv_bayes <- 
  function(nround, max.depth, min_child_weight, 
           eta, gamma, lambda, alpha) {
    
    # set parameters
    param <- list(booster = "gbtree",
                  max_depth = max.depth,
                  min_child_weight = min_child_weight,
                  eta = eta,
                  gamma = gamma,
                  subsample = 1,
                  colsample_bytree = 0.4,
                  max_delta_step = 0,
                  lambda = lambda,
                  alpha = alpha,
                  scale_pos_weight = (nrow(train_df) - sum(train_df$target_10w))/sum(train_df$target_10w),
                  objective = "binary:logistic",
                  eval_metric = "logloss")
    
    # cross validation
    cv <- xgb.cv(params = param,
                 data = dtrain,
                 folds = cv_folds,
                 watchlist = list(eval = test_df),
                 nrounds = 200,
                 early_stopping_rounds = 5,
                 maximize = F)
    
    list(Score = (-1) * cv$evaluation_log$test_logloss_mean[cv$best_iteration],
         Pred = cv$best_iteration)
  }

OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth = c(3L, 10L),
                                              min_child_weight = c(1L, 10L),
                                              eta = c(0.01, 0.3),
                                              lambda = c(10^c(-2:1)),
                                              alpha = c(0.1, 0.2, 0.5, 0.75, 1),
                                              gamma = c(0.0, 0.2)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = T)

# ----- Finalize XGboost Model -----
best_param <- list(
  booster = "gbtree",
  eval.metric = "logloss",
  objective = "binary:logistic",
  max_depth = OPT_Res$Best_Par["max.depth"],
  eta = OPT_Res$Best_Par["eta"],
  gamma = OPT_Res$Best_Par["gamma"],
  min_child_weight = OPT_Res$Best_Par["min_child_weight"],
  max_delta_step = OPT_Res$Best_Par["max_delta_step"])