library("multidplyr")
library("lubridate")

ts_setup <- "modelling_data_1.03_ret_20_wind"
board_nm <- "Portfolio Management"

# ----- Register Board -----
pins::board_register_local(name = board_nm)

# ----- Load Data Preparation -----
source("./1 variable preparation/Data Preparation for Prophet.R")
source("./2 modelling pipeline/Backtesting/dates of rebalancing.R")

# ----- Prepare Data for Prophet -----
raw_data <- prophetDataPreparator$new()
raw_data$add_pars(board_nm = board_nm, 
                  ma_list = c(20, 40, 60, 120))
raw_data$transform()

# ---- Load ADF and Hurst Results ----
stocks_selected <-
  pins::pin_get("adf_test_result", board_nm) %>% 
  dplyr::select(ticker, price_type, ma_type, test_interpretation) %>% 
  dplyr::rename(adf_interpretation = test_interpretation) %>% 
  as.data.frame() %>% 
  
  left_join(., pins::pin_get("hurst_result", board_nm) %>% 
              dplyr::select(ticker, price_type, ma_type, test_interpretation) %>% 
              dplyr::rename(hurst_interpretation = test_interpretation) %>% 
              as.data.frame()) %>% 
  
  # - good stocks based on ADF and Hurst
  rowwise() %>% 
  dplyr::mutate(
    adf_good = dplyr::if_else(!(adf_interpretation %in% c("Possible Random Walk", "Mean Reversion TS at 10%", "Mean Reversion TS at 5%")), 1, 0),
    hurst_good = dplyr::if_else(hurst_interpretation != "Possible Random Walk", 1, 0),
  ) %>% 
  dplyr::group_by(ticker) %>% 
  dplyr::mutate(
    n_rows = n(),
    adf = sum(adf_good),
    hurst = sum(hurst_good),
  ) %>% 
  rowwise() %>% 
  dplyr::mutate(is_good = dplyr::if_else(n_rows == adf & n_rows == hurst, 1, 0)) %>% 
  as.data.frame() %>% 
  
  dplyr::filter(is_good == 1) %>% 
  dplyr::select(ticker, price_type, ma_type) %>% 
  as.data.frame()

# ----- Subset Relevant Stocks -----
modelling_df <- 
  
  # - Get only relevant stocks
  stocks_selected %>% 
  left_join(., raw_data$modelling_df) %>% 
  dplyr::group_by(ticker, price_type, ma_type) %>%
  tidyr::nest() %>%
  dplyr::rename(prophet_data = data) %>% 
  
  # - get technical indicators
  left_join(., pins::pin_get(ts_setup, board_nm) %>% 
              dplyr::group_by(ticker) %>% 
              tidyr::nest()) %>% 
  dplyr::rename(technical_indicators_data = data) %>% 
  
  # - get rebalancing dates
  tidyr::crossing(., rebalancing_dates) %>% 
  dplyr::slice(1:1000) %>% 
  
  # - create train/test data
  dplyr::group_by(ticker, price_type, ma_type, created_at) %>%
  dplyr::mutate(
    
    train_df = purrr::map2(prophet_data, technical_indicators_data, function(df1, df2){
      
      df1 %>% 
        dplyr::filter(ds < created_at) %>% 
        dplyr::left_join(., df2, by = c("ds" = "dt")) %>% 
        as.data.frame()
      
    }),
    
    test_df = purrr::map2(prophet_data, technical_indicators_data, function(df1, df2){
      
      df1 %>% 
        dplyr::filter(ds >= created_at & 
                        ds < created_at %m+% months(1)) %>% 
        dplyr::left_join(., df2, by = c("ds" = "dt")) %>% 
        as.data.frame()
      
    }),
  ) %>% 
  as.data.frame()

# ----- Test Estimation of LSTM Model -----
library(keras)
library(tensorflow)
use_condaenv("ds_projects", required = T)

model_data <- 
  modelling_df$train_df[[1]] %>% 
  dplyr::select(-values, -open, -high, -low, -close, -volume, -p_adjusted) %>% 
  na.omit() %>% 
  as.data.frame()

train_x <- 
  model_data %>% 
  dplyr::select(-ds, -y, -target_10w) %>% 
  as.matrix()
  # dplyr::mutate(rowid = row_number()) %>% 
  # dplyr::group_by(rowid) %>% 
  # tidyr::nest() %>% 
  # dplyr::rename(x = data) %>%
  # dplyr::group_by(rowid) %>% 
  # dplyr::mutate(x_adj = purrr::map(x, function(df) df %>% as.numeric())) %>% 
  # dplyr::ungroup() %>% 
  # dplyr::select(-x, -rowid) %>% 
  # dplyr::rename(x = x_adj)
train_y <- 
  model_data %>% 
  dplyr::select(target_10w) %>% 
  as.matrix()

model <- keras_model_sequential()
model %>% 
  compile(optimizer = "adam",
          loss = "binary_crossentropy",
          metrics = c("acc"))

# - Bidirectional LSTM Model
model %>%
  layer_embedding(input_dim = 55, output_dim = 4) %>%
  layer_lstm(units = 4, return_sequences = TRUE) %>%
  layer_lstm(units = 4, return_sequences = TRUE) %>%
  bidirectional(layer_lstm(units = 4)) %>%
  layer_dense(units = 1, activation = "sigmoid")

# - compile model
model %>% 
  compile(optimizer = "adam",
          loss = "binary_crossentropy",
          metrics = c("acc"))

history <- 
  model %>% 
  fit(train_x, train_y,
      epochs = 25,
      # batch_size = 128,
      validation_split = 0.3)
