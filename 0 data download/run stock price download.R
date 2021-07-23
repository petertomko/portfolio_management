rstudioapi::jobRunScript(
  path = "./0 data download/stock price download.R", 
  name = "Download Price Data", 
  workingDir = paste(home_dir, "/portfolio_management/", sep = "")
)
