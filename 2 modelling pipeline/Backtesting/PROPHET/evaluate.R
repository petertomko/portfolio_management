library("ROI")
library("ROI.plugin.alabama")
library("tidyquant")
library("plotly")
library("tidyverse")

# ---- Load Estimated PROPHET Models -----
load("./tmp/estimated prophet model - tmp.RData")
source("./2 modelling pipeline/Backtesting/dates of rebalancing.R")

# ----- Register Board -----
pins::board_register_local(name = board_nm)

# ----- Select best model using RMSE -----
eval_prophet <-
  modelling_df %>% 
  
  # - compute performance metrics
  dplyr::group_by(ticker, price_type, year, month, created_at) %>% 
  dplyr::mutate(
    pick_model = purrr::map2(train_eval_df, data, function(tr_df, raw_df){
      
      # tr_df <- modelling_df$train_eval_df[[1]]
      # raw_df <- modelling_df$data[[1]]
      
      # - prepare data for metrics computation
      tmp_df <- 
        tr_df %>% 
        dplyr::arrange(ds) %>% 
        as.data.frame()
      
      # - evaluate metrics
      data.frame(
        
        # - standard deviation
        "sd_orig_price" = sd(tmp_df$y),
        "sd_pred" = sd(tmp_df$yhat),
        
        # - mean
        "mean_orig_price" = mean(tmp_df$y),
        "mean_pred" = mean(tmp_df$yhat),
        
        # - RMSE, MAPE, MAE
        "rmse_pred" = yardstick::rmse_vec(tmp_df$y, tmp_df$yhat),
        "mae_pred" = yardstick::mae_vec(tmp_df$y, tmp_df$yhat),
        "mape_pred" = yardstick::mape_vec(tmp_df$y, tmp_df$yhat)
      ) %>% 
        return()
    })
    
  ) %>% 
  dplyr::select(ticker, price_type, year, month, created_at, 
                pick_model) %>% 
  tidyr::unnest(c(pick_model)) %>% 
  as.data.frame() %>% 
  
  # - select model with minimal RMSE
  dplyr::group_by(ticker, year, month, created_at) %>% 
  dplyr::mutate(
    min_rmse_type = dplyr::if_else(rmse_pred == min(rmse_pred), 1, 0)
  ) %>% 
  dplyr::filter(min_rmse_type == 1) %>%
  dplyr::left_join(., modelling_df) %>% 
  
  # - calculate predicted returns
  dplyr::group_by(ticker, year, month, created_at) %>% 
  dplyr::mutate(
    pred_returns = purrr::map(test_eval_df, function(df){
      
      df %>% 
        dplyr::select(ds, yhat) %>% 
        dplyr::arrange(ds) %>% 
        dplyr::mutate(ret = (yhat/dplyr::lag(yhat, n = 1)) - 1) %>% 
        na.omit() %>% 
        dplyr::select(ds, ret)
      
    })
  )

# ----- Portfolio Optimization -----

# - select only one rebalancing date
asset_returns <-
  eval_prophet %>% 
  dplyr::ungroup() %>% 
  dplyr::select(ticker, created_at, pred_returns) %>% 
  tidyr::unnest(c(pred_returns)) %>%
  dplyr::filter(!(ticker %in% c("DPW", "SOAN"))) %>% 
  as.data.frame() %>% 
  
  # dplyr::filter(created_at == rebalancing_dates %>% 
  #                 dplyr::arrange(created_at) %>% 
  #                 dplyr::slice(1) %>% 
  #                 dplyr::pull(created_at)) %>% 
  
  dplyr::distinct() %>% 
  
  tidyr::spread(., ticker, ret) %>% 
  as.data.frame() %>% 
  
  dplyr::mutate_if(is.numeric, function(x) dplyr::if_else(is.na(x), 0, x)) %>% 
  as.data.frame()

portfolio_df <- list()
count <- 1

for (i in unique(asset_returns$created_at)) {
  
  print(i)
  
  cov_mtx <- 
    cov(asset_returns %>% 
          dplyr::filter(created_at == i) %>% 
          dplyr::select(-created_at, -ds))
  
  stats_tbl <- asset_returns %>%
    dplyr::filter(created_at == i) %>% 
    tidyr::gather(., ticker, daily_return, -created_at, -ds) %>% 
    dplyr::group_by(ticker) %>% 
    summarise(
      mean  = mean(daily_return),
      stdev = sd(daily_return)
    )
  
  calc_portfolio_variance <- function(weights) {
    t(weights) %*% (cov_mtx %*% weights) %>% as.vector()
  }
  
  calc_portfolio_return <- function(weights) {
    stats <- stats_tbl$mean
    sum(stats * weights)
  }
  
  n_assets <- nrow(stats_tbl)
  model_nlp <- OP(
    objective   = F_objective(F = calc_portfolio_return, 
                              n = n_assets, 
                              names = stats_tbl$ticker),
    constraints = rbind(
      F_constraint(F = calc_portfolio_variance, dir = "<=", rhs = 0.01),
      
      L_constraint(diag(n_assets), rep(">=", n_assets), rep(0, n_assets)),
      L_constraint(diag(n_assets), rep("<=", n_assets), rep(1, n_assets)),
      L_constraint(rep(1, n_assets), "==", 1)
    ),
    maximum = T
  )
  
  sol <- 
    ROI_solve(model_nlp, 
              solver = "alabama", 
              start = rep(1/n_assets, n_assets))
  
  portfolio_df[[count]] <- 
    data.frame("created_at" = i,
               "ticker" = names(sol$solution),
               "weight" = as.numeric(sol$solution))
  count <- count + 1
}

portfolio_tdf <-
  portfolio_df %>% 
  bind_rows() %>% 
  dplyr::mutate(created_at = as.Date(created_at),
                weight = round(weight, 4)) %>% 
  as.data.frame()

portfolio_returns_df <-
  
  modelling_df %>% 
  dplyr::ungroup() %>% 
  dplyr::select(ticker, created_at, test_df) %>% 
  tidyr::unnest(c(test_df)) %>% 
  dplyr::filter(!(ticker %in% c("DPW", "SOAN"))) %>% 
  as.data.frame() %>% 
  distinct() %>%
  dplyr::select(ticker, ds, created_at) %>% 
  as.data.frame() %>% 
  
  # - original prices
  dplyr::left_join(., pins::pin_get("md_stock_prices", board_nm) %>% 
                     dplyr::rename(ds = dt) %>% 
                     distinct() %>% 
                     
                     dplyr::group_by(ticker) %>% 
                     arrange(ds) %>% 
                     dplyr::mutate(
                       open_ret = (open/dplyr::lag(open, n = 1)) - 1,
                       high_ret = (high/dplyr::lag(high, n = 1)) - 1,
                       low_ret = (low/dplyr::lag(low, n = 1)) - 1,
                       close_ret = (close/dplyr::lag(close, n = 1)) - 1,
                       adj_ret = (p_adjusted/dplyr::lag(p_adjusted, n = 1)) - 1,
                     ) %>% 
                     dplyr::select(ticker, ds, open_ret, high_ret, 
                                   low_ret, close_ret, adj_ret) %>% 
                     as.data.frame()) %>% 
  
  # - portfolio results
  dplyr::left_join(., portfolio_tdf) %>% 
  
  dplyr::mutate(weight = dplyr::if_else(weight < 0.01, 0, weight)) %>% 
  dplyr::distinct() %>% 
  as.data.frame()

# ----- Overall Portfolio Daily Returns -----
portfolio_results_df <- 
  portfolio_returns_df %>% 
  dplyr::group_by(ds) %>% 
  dplyr::summarise(
    open_returns = sum(open_ret * weight),
    high_returns = sum(high_ret * weight),
    low_returns = sum(low_ret * weight),
    close_returns = sum(close_ret * weight),
    adj_returns = sum(adj_ret * weight),
    ) %>% 
  dplyr::ungroup() %>% 
  as.data.frame() %>% 
  
  dplyr::arrange(ds) %>% 
  dplyr::mutate(
    c_open_returns = cumsum(open_returns),
    c_high_returns = cumsum(high_returns),
    c_low_returns = cumsum(low_returns),
    c_close_returns = cumsum(close_returns),
    c_adj_returns = cumsum(adj_returns),
  ) %>% 
  as.data.frame()

# ----- Annualized Sharpe Ratios -----
sqrt(252) * mean(portfolio_results_df$open_returns)/(sd(portfolio_results_df$open_returns))
sqrt(252) * mean(portfolio_results_df$high_returns)/(sd(portfolio_results_df$high_returns))
sqrt(252) * mean(portfolio_results_df$low_returns)/(sd(portfolio_results_df$low_returns))
sqrt(252) * mean(portfolio_results_df$close_returns)/(sd(portfolio_results_df$close_returns))
sqrt(252) * mean(portfolio_results_df$adj_returns)/(sd(portfolio_results_df$adj_returns))

# ----- Portfolio weights by Shares -----
portfolio_weights <-
  portfolio_tdf %>% 
  dplyr::mutate(weight = dplyr::if_else(weight < 0.01, 0, weight)) %>% 
  tidyr::spread(., ticker, weight) %>% 
  as.data.frame()

# ----- Distinct shares invested in over time -----
portfolio_tdf %>% 
  dplyr::mutate(weight = dplyr::if_else(weight < 0.01, 0, weight)) %>% 
  dplyr::filter(weight > 0) %>% 
  dplyr::select(ticker) %>% 
  distinct()
