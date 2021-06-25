rstudioapi::jobRunScript(path = "0 data download/stock price download.R", 
                         name = "Download Daily Stock Prices", 
                         workingDir = "C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management")

rstudioapi::jobRunScript(path = "1 variable preparation/technical indicators.R", 
                         name = "Compute Technical Indicators", 
                         workingDir = "C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management")