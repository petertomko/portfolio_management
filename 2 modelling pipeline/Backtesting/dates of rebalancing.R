rebalancing_dates <- 
  expand.grid("year" = c(2018, 2019, 2020),
              "month" = c(1:12)) %>% 
  rowwise() %>% 
  
  dplyr::mutate(
    created_at = lubridate::dmy(paste("01-", month, '-', year, sep = ""))
    ) %>% 
  
  as.data.frame()