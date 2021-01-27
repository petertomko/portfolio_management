library("readr")
library("readxl")

list_cp <- read_excel("0 data download/patria tickers/MarginCP.xlsx", skip = 2)

# set dates
first.date <- Sys.Date() - 360 * 2
last.date <- Sys.Date()
freq.data <- "daily"

# set tickers
tickers <- list_cp$Ticker

# download
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), "_cache"))

kept_tickers <- 
  l.out$df.control %>% 
  dplyr::filter(threshold.decision %in% "KEEP") %>% 
  dplyr::pull(ticker)

# pre processing
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

# save to pins
master_data <- 
  master_data %>% 
  as.data.frame() %>%   
  pin("md_stock_prices", "local")
