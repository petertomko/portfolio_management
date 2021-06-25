library(h2o)

# TRAIN_DT <- as.Date("2021-01-01")

h2o.init()

# ----- Register Board -----
pins::board_register_local(name = "Portfolio Management")

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data", "Portfolio Management") %>% 
  as.data.frame() %>% 
  as.h2o()

modelling_data_split <- 
  h2o.splitFrame(modelling_data, 
                 ratios = c(0.6, 0.2), seed = 1234)

modelling_data_train <- modelling_data_split[[1]]
modelling_data_valid <- modelling_data_split[[2]]
modelling_data_test <- modelling_data_split[[3]]

hyper_params <- 
  list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", 
                      "MaxoutWithDropout", "TanhWithDropout"), 
       hidden = list(c(50, 50, 50, 50), 
                     c(200, 200), 
                     c(200, 200, 200), 
                     c(200, 200, 200, 200)), 
       epochs = c(50, 100, 200), 
       l1 = c(0, 0.00001, 0.0001), 
       l2 = c(0, 0.00001, 0.0001), 
       adaptive_rate = c(TRUE, FALSE), 
       rate = c(0, 0.1, 0.005, 0.001), 
       rate_annealing = c(1e-8, 1e-7, 1e-6), 
       rho = c(0.9, 0.95, 0.99, 0.999), 
       epsilon = c(1e-10, 1e-8, 1e-6, 1e-4), 
       momentum_start = c(0, 0.5),
       momentum_stable = c(0.99, 0.5, 0), 
       input_dropout_ratio = c(0, 0.1, 0.2)
  )

search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 10 * 3600,
                        max_models = 100,
                        stopping_metric = "AUC", 
                        stopping_tolerance = 0.00001, 
                        stopping_rounds = 5, 
                        seed = 1234
)

models_dl <- h2o.grid(algorithm = "deeplearning", 
                      grid_id = "grd_dl", 
                      x = predictors, 
                      y = "target_10w", 
                      training_frame = modelling_data_train, 
                      validation_frame = modelling_data_valid, 
                      nfolds = 0, hyper_params = hyper_params, 
                      search_criteria = search_criteria, 
                      stopping_metric = "AUC", 
                      stopping_tolerance = 1e-3, 
                      stopping_rounds = 2, seed = 1234)

models_dl_sort <- h2o.getGrid(grid_id = "grd_dl", sort_by = "auc", decreasing = TRUE)
models_dl_best <- h2o.getModel(models_dl_sort@model_ids[[1]])