if(as.character(Sys.info()["nodename"]) == "ND0151MB"){
  
  rstudioapi::jobRunScript(
    path = "./0 data download/stock price download.R", 
    name = "Download Price Data", 
    workingDir = "C:/Users/peter.tomko/OneDrive - Publicis Groupe/Desktop/ds_projects/portfolio_management/"
  )
  
}else{
  
  rstudioapi::jobRunScript(
    path = "./0 data download/stock price download.R", 
    name = "Download Price Data", 
    workingDir = "C:/Users/Peter/Desktop/ds_projects/portfolio_management"
  )
}
