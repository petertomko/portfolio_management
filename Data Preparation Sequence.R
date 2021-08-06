source("./0 data download/stock price download.R")
source("./1 variable preparation/technical indicators.R")

# - Run ADF Test
rmarkdown::render(input = "./adhoc analysis/ADF Test.Rmd", 
                  output_file = "./adhoc analysis/ADF-Test.html")

# - Run Hurst Exponent Computation
rmarkdown::render(input = "./adhoc analysis/Stationarity Test using Hurst Exponent.Rmd", 
                  output_file = "./adhoc analysis/Stationarity-Test-using-Hurst-Exponent.html")
