# ----- Connect to Board -----
pins::board_register_local(name = board_nm)

# ----- Load Data and Compute Technical Indicators -----
modelling_list <- list()
for (i in 1:nrow(data_setup)) {
  
  # - Download Data
  tmp_data <- 
    
    pins::pin_get(paste("modelling_data", "_", 
                        data_setup$return_target[i], 
                        "_ret_", data_setup$return_window[i], 
                        "_wind", sep = ""), board_nm) %>% 
    as.data.frame() %>% 
    
    dplyr::mutate(
      return_target = data_setup$return_target[i], 
      return_window = data_setup$return_window[i]
    ) %>% 
    as.data.frame()
  
  # - Insert into list
  modelling_list[[i]] <- tmp_data
  
  print(i)
}

modelling_data <-
  modelling_list %>% 
  bind_rows() %>%
  as.data.frame()

rm(i)
rm(tmp_data)
rm(modelling_list)
gc()
