library("multidplyr")
library("lubridate")

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
  dplyr::select(ticker, price_type, test_interpretation) %>% 
  dplyr::rename(adf_interpretation = test_interpretation) %>% 
  as.data.frame() %>% 
  
  left_join(., pins::pin_get("hurst_result", board_nm) %>% 
              dplyr::select(ticker, price_type, test_interpretation) %>% 
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
  dplyr::select(ticker, price_type) %>% 
  as.data.frame()

# ----- Subset Relevant Stocks -----
modelling_df <- 
  
  # - Get only relevant stocks
  stocks_selected %>% 
  left_join(., raw_data$modelling_df) %>% 
  dplyr::group_by(ticker, price_type) %>%
  tidyr::nest() %>%
  
  # - get rebalancing dates
  tidyr::crossing(., rebalancing_dates) %>% 
  
  # - create train/test data
  dplyr::group_by(ticker, price_type, created_at) %>%
  dplyr::mutate(
    train_df = purrr::map(data, function(df){
      
      df %>% 
        dplyr::filter(ds < created_at) %>% 
        as.data.frame()
      
    }),
    test_df = purrr::map(data, function(df){
      
      df %>% 
        dplyr::filter(ds >= created_at & 
                        ds < created_at %m+% months(1)) %>% 
        as.data.frame()
      
    }),
  ) %>% 
  as.data.frame()

# ----- Estimate -----
cluster <- multidplyr::new_cluster(8)
multidplyr::cluster_library(cluster, c("prophet", "data.table", "dplyr"))
modelling_df <-
  modelling_df %>% 
  # slice(1:10) %>%
  dplyr::group_by(ticker, price_type, created_at) %>%
  partition(cluster) %>% 
  dplyr::mutate(
    model = purrr::map(train_df, function(df){
      
      # df <- modelling_df$train_df[[1]]
      
      m <- prophet::prophet()
      m <- add_regressor(m, "ma_type_20", mode = "additive")
      m <- add_regressor(m, "ma_type_40", mode = "additive")
      m <- add_regressor(m, "ma_type_60", mode = "additive")
      m <- add_regressor(m, "ma_type_120", mode = "additive")
      m <- fit.prophet(m, df)
      
      return(m)
    })
  ) %>% 
  collect()

# ----- Predict -----
modelling_df <-
  modelling_df %>% 
  dplyr::group_by(ticker, price_type, created_at) %>%
  partition(cluster) %>% 
  dplyr::mutate(
    train_eval_df = purrr::map2(model, train_df, function(mdl, df){
      
      df %>% 
        left_join(., predict(mdl, 
                             df = df %>%
                               dplyr::select(ds, ma_type_20, ma_type_40, ma_type_60, ma_type_120) %>% 
                               as.data.frame()) %>% 
                    dplyr::select(-ma_type_20, -ma_type_40, -ma_type_60, -ma_type_120) %>% 
                    as.data.frame()
        ) %>% 
        as.data.frame()
      
    }),
    
    test_eval_df = purrr::map2(model, test_df, function(mdl, df){
      
      df %>% 
        left_join(., predict(mdl, 
                             df = df %>%
                               dplyr::select(ds, ma_type_20, ma_type_40, ma_type_60, ma_type_120) %>% 
                               as.data.frame()) %>% 
                    dplyr::select(-ma_type_20, -ma_type_40, -ma_type_60, -ma_type_120) %>% 
                    as.data.frame()
        ) %>% 
        as.data.frame()
      
    }),
  ) %>% 
  collect()

# ----- Write to Temporary Folder -----
save(modelling_df, file = "./tmp/estimated prophet model - tmp.RData")