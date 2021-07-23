library(lubridate)

board_nm <- "Portfolio Management"

# ----- Register Board -----
pins::board_register_local(name = board_nm)

# ----- Load Data Preparator -----
source("C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management/1 variable preparation/Data Preparation for Prophet.R")
source("C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management/2 modelling pipeline/Backtesting/dates of rebalancing.R")

# - Load Raw Data
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
  
  # - get rebalancing dates
  tidyr::crossing(., rebalancing_dates) %>% 
  
  # - create train/test data
  dplyr::group_by(ticker, price_type, ma_type, created_at) %>%
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

# ----- Estimate Prophet Model -----
library(multidplyr)
cluster <- multidplyr::new_cluster(4)
multidplyr::cluster_library(cluster, c("prophet", "data.table", "dplyr"))
modelling_df <-
  modelling_df %>% 
  dplyr::group_by(ticker, price_type, ma_type, created_at) %>%
  partition(cluster) %>% 
  dplyr::mutate(
    prophet_model = purrr::map(train_df, function(df){
      
      prophet::prophet(
        df = df, growth = "linear", fit = T
      )
      
    })
  ) %>% 
  collect()

# ----- Predict Model -----
modelling_df <-
  modelling_df %>% 
  dplyr::group_by(ticker, price_type, ma_type, created_at) %>%
  dplyr::mutate(
    eva_df = purrr::map2(prophet_model, test_df, function(mdl, df){
      
      df %>% 
        left_join(., predict(mdl, 
                             df = df %>%
                               dplyr::select(ds) %>% 
                               as.data.frame()) %>% 
                    as.data.frame()
        ) %>% 
        as.data.frame()
      
    })
  )
