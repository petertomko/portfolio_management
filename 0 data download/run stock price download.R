if(as.character(Sys.info()["nodename"]) == "ND0151MB"){
  
  rstudioapi::jobRunScript(
    path = "./0 data download/stock price download.R", 
    name = "Download Price Data", 
    workingDir = "C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management/"
  )
}

if(as.character(Sys.info()["nodename"]) == "copernicus64gb"){
  
  rstudioapi::jobRunScript(
    path = "/home/azureuser/cloudfiles/code/portfolio_management/0 data download/stock price download.R", 
    name = "Download Price Data", 
    workingDir = "/home/azureuser/cloudfiles/code/portfolio_management"
  )
}
