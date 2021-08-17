list_cp <- 
  read_excel("0 data download/patria tickers/MarginCP.xlsx", 
             skip = 2) %>% 
  dplyr::mutate(`Adjusted Ticker` = Ticker) %>% 
  dplyr::relocate(., `Adjusted Ticker`, .after = Ticker) %>% 
  pins::pin(x = ., name = "MarginCP", board = board_nm)
