# ------ Load Library -----
library("lightgbm")

TRAIN_DT <- as.Date("2021-01-01")

# ----- Register Board -----
pins::board_register_local(name = "Portfolio Management")

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data", "Portfolio Management") %>% 
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

rm(modelling_data)
rm(train_df)
rm(test_df)
gc()

nrounds <- 2L
param <- list(
  num_leaves = 4L
  , learning_rate = 1.0
  , objective = "binary"
)

print("Running cross validation")
# Do cross validation, this will print result out as
# [iteration]  metric_name:mean_value+std_value
# std_value is standard deviation of the metric
lgb_model <- 
  lgb.cv(
    param
    , dtrain
    , nrounds
    , nfold = 5L
    , eval = "binary_error"
  )
