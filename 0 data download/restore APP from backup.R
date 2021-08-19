# ----- Load Backup RData file -----
file_path <- "C:/Users/peter.tomko/Downloads/portfolio_manager_backup.RData"
load(file_path)

# ----- Write to pins board -----
pins::board_register_local(name = board_nm)

# - md_stock_prices
pins::pin(x = md_stock_prices,
          name = "md_stock_prices",
          board = board_nm, 
          description = "Stock Price Data")

# - MarginCP
pins::pin(x = MarginCP,
          name = "MarginCP",
          board = board_nm)
