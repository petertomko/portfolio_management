source("1 variable preparation/combine datasets.R")

# ------ Load Library -----
library("lightgbm")

# ----- Set Date for Training -----
TRAIN_DT <- as.Date("2021-01-01")

# ----- Register Board -----
pins::board_register_local(name = board_nm)

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data_1.03_ret_10_wind", board_nm) %>% 
  as.data.frame() %>% 
  
  # - split train/test
  dplyr::mutate(
    sample_type = dplyr::if_else(dt < TRAIN_DT, "Train", "Test")
  ) %>% 
  
  # - Remove columns
  dplyr::select(-dt, -ticker, 
                -open, -high, -low, -close, -p_adjusted, -volume) %>% 
  as.data.frame()

train_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Train") %>% 
  dplyr::select(-sample_type) %>% 
  as.data.frame()

test_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Test") %>% 
  dplyr::select(-sample_type) %>% 
  as.data.frame()

# load data into lgb format
dtrain <- lgb.Dataset(train_df %>% 
                        dplyr::select(-target_10w) %>% 
                        as.matrix(), 
                      label = train_df$target_10w)

dtest <- lgb.Dataset.create.valid(dtrain, 
                                  data = test_df %>% 
                                    dplyr::select(-target_10w) %>% 
                                    as.matrix(), 
                                  label = test_df$target_10w)

nrounds <- 200L
param <- list(
  num_leaves = 4L
  , learning_rate = 1.0
  , objective = "binary"
)

print("Running cross validation")

lgb_model <- 
  lgb.train(
    param
    , dtrain
    , valids = list(test = dtest)
    , nrounds
    , nfold = 20L
    , eval = "average_precision"
    , early_stopping_rounds = 5
    , stratified = TRUE
  )

test_predict <- predict(lgb_model, 
                        test_df %>% 
                          dplyr::select(-target_10w) %>% 
                          as.matrix())

test_df$pred_prob <- test_predict

test_df <- 
  test_df %>% 
  dplyr::mutate(pred_lbl = dplyr::if_else(pred_prob > 0.55, "yes", "no")) %>% 
  as.data.frame()

table(test_df$target_10w, test_df$pred_lbl)
gc()
