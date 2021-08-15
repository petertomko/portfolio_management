# ----- Data Preparation for PROPHET Model -----
prophetDataPreparator <- R6::R6Class(
  "prophetDataPreparator",
  cloneable = F,
  
  public = list(
    
    board_nm = NULL,
    ma_list = NULL,
    modelling_df = NULL,
    
    # add parameters
    add_pars = function(board_nm, ma_list){
      self$board_nm = board_nm
      self$ma_list = ma_list
    },
    
    # transform data
    transform = function(){
      
      raw_data <-
        
        # - Download Data
        pins::pin_get("md_stock_prices", self$board_nm) %>% 
        # dplyr::filter(ticker %in% c("AAL", "X")) %>%
        as.data.frame() %>% 
        
        # - Select specific ticker
        dplyr::filter(volume > 0) %>% 
        
        # - Gather to create price type column
        dplyr::select(dt, ticker, open, high, low, close, p_adjusted) %>% 
        tidyr::gather(., "price_type", "values", -dt, -ticker) %>% 
        dplyr::distinct() %>% 
        as.data.frame() %>% 
        
        dplyr::group_by(ticker, price_type) %>%
        tidyr::nest() %>% 
        
        # - Expand by ma_list
        tidyr::crossing(., data.frame("ma_type" = self$ma_list)) %>% 
        
        # - Calculate Moving Averages
        dplyr::group_by(ticker, price_type, ma_type) %>% 
        dplyr::mutate(
          ma_price_df = purrr::map2(data, ma_type, function(df, ma){
            
            df %>% 
              dplyr::ungroup() %>% 
              dplyr::arrange(dt) %>% 
              dplyr::mutate(
                ma_price = 
                  dplyr::lag(zoo::rollmean(values, 
                                           k = ma, 
                                           fill = NA, 
                                           align = "right"), 
                             n = (ma + 1))) %>% 
              as.data.frame()
            
          })
        ) %>% 
        dplyr::select(-data) %>% 
        tidyr::unnest(c(ma_price_df)) %>% 
        rowwise() %>% 
        dplyr::mutate(ma_type = paste("ma_type_", ma_type, sep = "")) %>%
        dplyr::ungroup() %>% 
        as.data.frame() %>% 
        
        tidyr::spread(., ma_type, ma_price) %>% 
        as.data.frame() %>% 
        
        # - rename to fit PROPHET model
        dplyr::rename(ds = dt,
                      y = values) %>% 
        na.omit() %>% 
        as.data.frame()
      
      self$modelling_df = raw_data
    }
    
  )
)
