# ----- Loading Library -----
library("tidyquant")
library("BatchGetSymbols")
library("readr")
library("readxl")
library("dplyr")
library("data.table")

# ----- Connect to Board -----
pins::board_register_local(name = board_nm)

# ----- Read Tickers -----
list_cp <- 
  read_excel("0 data download/patria tickers/MarginCP.xlsx", 
             skip = 2) %>% 
  pins::pin(x = ., name = "MarginCP", board = board_nm)

# ----- set dates -----
first.date <- as.Date("2016-01-01")
last.date <- Sys.Date()
freq.data <- "daily"

# ----- Set Tickers -----
tickers <- list_cp$Ticker

# ----- Download -----
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), "_cache"))

# ----- Subset Tickers - successful download -----
kept_tickers <- 
  l.out$df.control %>% 
  dplyr::filter(threshold.decision %in% "KEEP") %>% 
  dplyr::pull(ticker)

# ----- Initial Pre-processing -----
master_data <- 
  l.out$df.tickers %>% 
  dplyr::filter(ticker %in% kept_tickers) %>% 
  dplyr::select(-ret.adjusted.prices, -ret.closing.prices) %>% 
  dplyr::rename(open = price.open,
                high = price.high,
                low = price.low,
                close = price.close,
                p_adjusted = price.adjusted,
                dt = ref.date) %>% 
  as.data.frame()

# ----- Get Tickers present in Last Trading Day -----
latest_tickers <- 
  
  master_data %>% 
  dplyr::filter(dt %in% max(master_data$dt)) %>% 
  as.data.frame() %>% 
  
  dplyr::pull(ticker) %>% 
  as.character() %>% 
  unique()

# ----- Subset Master Data -----
modelling_data <- 
  master_data %>% 
  dplyr::filter(ticker %in% latest_tickers) %>% 
  as.data.frame() %>% 
  
  # get min and max date
  dplyr::group_by(ticker) %>% 
  dplyr::summarise(min_dt = min(dt),
                   max_dt = max(dt)) %>% 
  as.data.frame() %>% 
  
  # count days between
  dplyr::mutate(count_days = as.numeric(max_dt - min_dt)) %>% 
  tidyr::uncount(., count_days) %>% 
  as.data.frame() %>% 
  
  # create date
  dplyr::group_by(ticker) %>% 
  dplyr::mutate(dt = min_dt + row_number() - 1) %>% 
  
  # de select
  dplyr::select(-min_dt, -max_dt) %>% 
  
  # join master data
  left_join(., master_data) %>% 
  
  # replace missing volumes
  dplyr::mutate(volume = dplyr::if_else(is.na(volume), 0, volume)) %>% 
  
  # backward fill
  dplyr::group_by(ticker) %>% 
  dplyr::arrange(dt) %>% 
  tidyr::fill(everything(), .direction = "down") %>% 
  
  # forward fill
  dplyr::group_by(ticker) %>% 
  dplyr::arrange(dt) %>% 
  tidyr::fill(everything(), .direction = "up") %>% 
  
  # save to pin
  as.data.frame() %>% 
  pins::pin(x = .,
            name = "md_stock_prices",
            board = board_nm, 
            description = "Stock Price Data")
