rebalancing_dates <- 
  expand.grid("year" = c(2018, 2019, 2020, 2021),
              "month" = c(1:12)) %>% 
  rowwise() %>% 
  
  dplyr::mutate(
    created_at = lubridate::dmy(paste("01-", month, '-', year, sep = ""))
    ) %>% 
  
  dplyr::filter(created_at <= as.Date("2021-07-01")) %>% 
  as.data.frame()