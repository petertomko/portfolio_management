# ----- Load Data and Compute Technical Indicators -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("md_stock_prices", "Portfolio Management") %>% 
  as.data.frame() %>% 
  
  # - Filter zero volume days
  dplyr::filter(volume > 0) %>% 
  as.data.frame() %>% 
  
  # - Number of observations per ticker
  dplyr::group_by(ticker) %>% 
  dplyr::mutate(n_obs = n()) %>%
  as.data.frame() %>% 
  
  # - Filter tickers with more than 100 observations
  dplyr::filter(n_obs > 100) %>% 
  dplyr::select(-n_obs) %>% 
  as.data.frame() %>% 
  
  # - Technical Indicators
  dplyr::group_by(ticker) %>% 
  dplyr::arrange(dt) %>% 
  dplyr::mutate(
    
    # RSI
    rsi_05 = TTR::RSI(close, n = 5),
    rsi_10 = TTR::RSI(close, n = 10),
    rsi_30 = TTR::RSI(close, n = 30),
    rsi_60 = TTR::RSI(close, n = 60),
    
    # MACD
    macd_5_10 = TTR::MACD(x = close, nFast = 5, nSlow = 10)/close,
    macd_5_30 = TTR::MACD(x = close, nFast = 5, nSlow = 30)/close,
    macd_5_60 = TTR::MACD(x = close, nFast = 5, nSlow = 60)/close,
    
    macd_10_30 = TTR::MACD(x = close, nFast = 10, nSlow = 30)/close,
    macd_10_60 = TTR::MACD(x = close, nFast = 10, nSlow = 60)/close,
    
    macd_30_60 = TTR::MACD(x = close, nFast = 30, nSlow = 60)/close,
    
    # MACD change
    abs_ch_macd_5_10 = lag(macd_5_10, n = 1) - macd_5_10,
    abs_ch_macd_5_30 = lag(macd_5_30, n = 1) - macd_5_30,
    abs_ch_macd_5_60 = lag(macd_5_60, n = 1) - macd_5_60,
    abs_ch_macd_10_30 = lag(macd_10_30, n = 1) - macd_10_30,
    abs_ch_macd_10_60 = lag(macd_10_60, n = 1) - macd_10_60,
    abs_ch_macd_30_60 = lag(macd_30_60, n = 1) - macd_30_60,
    
    perc_ch_macd_5_10 = (lag(macd_5_10, n = 1) - macd_5_10)/macd_5_10,
    perc_ch_macd_5_30 = (lag(macd_5_30, n = 1) - macd_5_30)/macd_5_30,
    perc_ch_macd_5_60 = (lag(macd_5_60, n = 1) - macd_5_60)/macd_5_60,
    perc_ch_macd_5_10 = (lag(macd_10_30, n = 1) - macd_10_30)/macd_10_30,
    perc_ch_macd_5_10 = (lag(macd_10_60, n = 1) - macd_10_60)/macd_10_60,
    perc_ch_macd_5_10 = (lag(macd_30_60, n = 1) - macd_30_60)/macd_30_60,
    
    # Returns
    d_ret_5 = runMean(close/lag(close, n = 1), n = 5),
    d_ret_10 = runMean(close/lag(close, n = 1), n = 10),
    d_ret_30 = runMean(close/lag(close, n = 1), n = 30),
    d_ret_60 = runMean(close/lag(close, n = 1), n = 60),
    
    w_ret_5 = runMean(close/lag(close, n = 7), n = 5),
    w_ret_10 = runMean(close/lag(close, n = 7), n = 10),
    w_ret_30 = runMean(close/lag(close, n = 7), n = 30),
    w_ret_60 = runMean(close/lag(close, n = 7), n = 60),
    
    m_ret_5 = runMean(close/lag(close, n = 30), n = 5),
    m_ret_10 = runMean(close/lag(close, n = 30), n = 10),
    m_ret_30 = runMean(close/lag(close, n = 30), n = 30),
    m_ret_60 = runMean(close/lag(close, n = 30), n = 60),
    
    # Close Price Ratio
    d_avgclose_close_5 = lag(runMean(close, n = 5))/close,
    d_avgclose_close_10 = lag(runMean(close, n = 10))/close,
    d_avgclose_close_30 = lag(runMean(close, n = 30))/close,
    d_avgclose_close_60 = lag(runMean(close, n = 60))/close,
    
    w_avgclose_close_5 = lag(runMean(close, n = 5))/close,
    w_avgclose_close_10 = lag(runMean(close, n = 10))/close,
    w_avgclose_close_30 = lag(runMean(close, n = 30))/close,
    w_avgclose_close_60 = lag(runMean(close, n = 60))/close,
    
    m_avgclose_close_5 = lag(runMean(close, n = 5))/close,
    m_avgclose_close_10 = lag(runMean(close, n = 10))/close,
    m_avgclose_close_30 = lag(runMean(close, n = 30))/close,
    m_avgclose_close_60 = lag(runMean(close, n = 60))/close,
    
    # Volume
    d_vol_5 = runMean(volume/lag(volume, n = 1), n = 5),
    d_vol_10 = runMean(volume/lag(volume, n = 1), n = 10),
    d_vol_30 = runMean(volume/lag(volume, n = 1), n = 30),
    d_vol_60 = runMean(volume/lag(volume, n = 1), n = 60),
    
    w_vol_5 = runMean(volume/lag(volume, n = 7), n = 5),
    w_vol_10 = runMean(volume/lag(volume, n = 7), n = 10),
    w_vol_30 = runMean(volume/lag(volume, n = 7), n = 30),
    w_vol_60 = runMean(volume/lag(volume, n = 7), n = 60),
    
    m_vol_5 = runMean(volume/lag(volume, n = 30), n = 5),
    m_vol_10 = runMean(volume/lag(volume, n = 30), n = 10),
    m_vol_30 = runMean(volume/lag(volume, n = 30), n = 30),
    m_vol_60 = runMean(volume/lag(volume, n = 30), n = 60)
  ) %>% 
  as.data.frame()

# ----- Create Target Variable -----
target_var <- 
  
  modelling_data %>% 
  dplyr::select(dt, ticker, close) %>% 
  as.data.frame() %>% 
  
  dplyr::left_join(., modelling_data %>% 
                     dplyr::select(dt, ticker, close) %>% 
                     dplyr::rename(lead_dt = dt,
                                   lead_close = close) %>% 
                     as.data.frame()) %>% 
  as.data.frame()

dplyr::group_by(ticker, dt) %>% 
  dplyr::mutate(
    target_var = purrr::map2(ticker, dt, function(sym, t_dt){
      
      # test
      # sym <- "AAL"
      # t_dt <- as.Date("2016-04-20")
      
      # get initial price
      init_price <- 
        modelling_data %>% 
        dplyr::filter(ticker %in% sym & dt %in% t_dt) %>% 
        pull(close)
      
      # subset data
      tmp_df <- 
        modelling_data %>% 
        dplyr::filter(ticker %in% sym & dt > t_dt) %>% 
        dplyr::select(dt, ticker, close, volume) %>% 
        as.data.frame() %>% 
        
        tidyr::crossing(., data.frame(target_w = c(10, 20, 40, 60))) %>% 
        as.data.frame() %>% 
        
        group_by(target_w) %>% 
        nest() %>% 
        
        dplyr::group_by(target_w) %>% 
        dplyr::mutate(
          price_ch = purrr::map2(target_w, data, function(w, df){
            
            # - pull prices
            pulled_prices <- 
              df %>% 
              dplyr::arrange(dt) %>% 
              slice(1:w) %>% 
              pull(close)
            
            # - create condition
            p_cond <- (pulled_prices/init_price)
            
            # - check condition
            p_cond_check <-
              data.frame(
                target_ch_1 = dplyr::if_else(sum(p_cond >= 1.01) > 0, 1, 0),
                target_ch_25 = dplyr::if_else(sum(p_cond >= 1.025) > 0, 1, 0),
                target_ch_5 = dplyr::if_else(sum(p_cond >= 1.05) > 0, 1, 0),
                
                sd_price = sd(pulled_prices)/init_price
              )
          })
        ) %>% 
        dplyr::select(-data) %>% 
        tidyr::unnest(c(price_ch))
      
      return(tmp_df)
    })
  ) %>% 
  tidyr::unnest(c(target_var)) %>% 
  as.data.frame()
