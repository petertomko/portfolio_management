library("tidyverse")
library("quantmod")
library("BatchGetSymbols")

# - Get all tickers
sap_500 <- BatchGetSymbols::GetSP500Stocks()
ftse_100 <- BatchGetSymbols::GetFTSE100Stocks()
names(sap_500) <- tolower(names(sap_500))

tickers_all <- 
  sap_500 %>% select(tickers, company) %>%
  rbind(., ftse_100 %>% select(tickers, company))

tickers_all <- tickers_all[1:10,]

master_data <- tickers_all %>% unnest(c(data))

# - set first & end date
end_date <- Sys.Date() - 1
first_date <- as.Date("1960-01-01")

tickers_all <- tickers_all %>%
  group_by(tickers) %>%
  mutate(data = map(tickers, function(i_tick){
    
    get.clean.data(i_tick, 
                   first.date = first_date, 
                   last.date = end_date)
  }))

# - setting dates (Test)
test_date <- as.Date("2020-09-01")
val_date <- as.Date("2019-01-01")
train_date <- as.Date("2017-01-01")

# - set number of trading days to compute return
return_period <- 20
min_point_rp <- 100
threshold_ratio <- 0.1

main_files <- 
  expand_grid("bin_ret" = 0.05,
              "tau" = c(1:10),
              "m" = c(2:6))

# - create main folder 
project_path <- "C:/Users/Peter/Desktop/ds_projects/portfolio_management"
main_folder <- "master_data"

dir.create(file.path(paste(project_path, "/", main_folder, sep = "")), 
           showWarnings = F)

for(i in 1:nrow(main_files)){
  
  # i <- 1
  # - main folder
  path_get <- paste(project_path, "/", main_folder, sep = "")
  
  # - create folder as combination with - target, tau, m
  sub_file <- paste("data_", main_files$bin_ret[i], "_", 
                    main_files$tau[i], "_", 
                    main_files$m[i], sep = "")
  dir.create(file.path(paste(path_get, "/", sub_file, sep = "")))
  
  # - create validation, test and train
  dir.create(file.path(paste(path_get, "/", sub_file, "/test", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/train", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/validation", sep = "")))
  
  # - create binary classification folders (Under & Over)
  dir.create(file.path(paste(path_get, "/", sub_file, "/test/under", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/train/under", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/validation/under", sep = "")))
  
  dir.create(file.path(paste(path_get, "/", sub_file, "/test/over", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/train/over", sep = "")))
  dir.create(file.path(paste(path_get, "/", sub_file, "/validation/over", sep = "")))
}

# - set first & end date
end_date <- Sys.Date() - 1
first_date <- as.Date("1960-01-01")

count <- 0
for(tick in tickers_all$tickers){
  
  print(tick)
  # tick <- "AAPL"
  
  # - download data
  temp_df <- get.clean.data(tick, first.date = first_date, last.date = end_date)
  
  # - process data
  temp_df <- 
    temp_df %>%
    na.omit() %>%
    as.data.frame() %>%
    crossing(., data.frame("bin_ret" = 0.05)) %>%
    
    # - calculate actual returns
    group_by(bin_ret) %>%
    arrange(ref.date) %>%
    mutate(ret_price = 
             `price.adjusted`/lag(`price.adjusted`, return_period) - 1) %>%
    
    # - create target variable
    mutate(bin_target = ifelse(lead(ret_price, return_period) > bin_ret, 1, 0),
           ret_target = lead(ret_price, return_period))  %>%
    filter(!is.na(bin_target)) %>%
    
    # - create month and year
    mutate(year = lubridate::year(ref.date),
           month = lubridate::month(ref.date),
           mweek = ceiling(lubridate::day(ref.date)/7)) %>%
    group_by(year, month, mweek) %>%
    mutate(last_day = max(ref.date)) %>%
    mutate(is_last = ifelse(last_day == ref.date, 1, 0)) %>%
    
    # - recurrent plot ID
    as.data.frame() %>%
    mutate(plot_id_start = 
             ifelse(min_point_rp + return_period >= row_number(), 0, 1)) %>%
    mutate(plot_id = cumsum(is_last) * is_last * plot_id_start) %>%
    mutate(plot_id = ifelse(plot_id == 0, NA, plot_id))
  
  # - generate recurrence plot
  min_id <- min(temp_df$plot_id, na.rm = T)
  max_id <- max(temp_df$plot_id, na.rm = T) - return_period
  
  # - 
  ctrl_df <- main_files %>%
    crossing(., data.frame("plot_id" = na.omit(unique(temp_df$plot_id))))
  
  for(i in 1:nrow(ctrl_df)){
    
    # i <- 1
    date_input <- 
      temp_df %>% 
      filter(plot_id == ctrl_df$plot_id[i] & bin_ret == ctrl_df$bin_ret[i]) %>% 
      pull(ref.date)
    
    # - subset data relevant for recurrent analysis
    temp_return <- 
      temp_df %>% 
      filter(ref.date <= date_input & bin_ret == ctrl_df$bin_ret[i]) %>% 
      arrange(desc(ref.date))
    temp_return <- temp_return[1:min_point_rp,]
    temp_return <- temp_return %>% arrange(ref.date)
    
    # - embedding one dimensional time series into phase space
    temp_ps <- nonlinearTseries::buildTakens(temp_return$ret_price,
                                             embedding.dim = ctrl_df$m[i], 
                                             time.lag = ctrl_df$tau[i])
    
    # - get distance matrix
    dist_df <- dist(temp_ps) %>% as.matrix()
    mean_threshold <- threshold_ratio * max(max(dist_df))
    
    # - get recurrence plot
    rec_plot <- dist_df * 0
    rec_plot[dist_df <= mean_threshold] <- 1
    
    # - get binary target
    bin_folder <- temp_return$bin_target[nrow(temp_return)]
    
    # - create file name
    if(date_input < train_date){
      if(bin_folder == 0){
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/train/under/tr_pic_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }else{
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/train/over/tr_pic_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }
    }
    
    if(date_input >= train_date & date_input < val_date){
      if(bin_folder == 0){
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/validation/under/v_pic_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }else{
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/validation/over/v_picture_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }
    }
    
    if(date_input >= val_date & date_input < test_date){
      if(bin_folder == 0){
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/test/under/te_pic_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }else{
        file_name <- paste(project_path, "/", main_folder, "/data_", 
                           ctrl_df$bin_ret[i], "_", ctrl_df$tau[i], "_", 
                           ctrl_df$m[i], "/test/over/te_picture_", 
                           count, "_", tolower(tick), ".png", sep = "")
        count <- count + 1
      }
    }
    
    # - save file
    png(filename = file_name)
    image(rec_plot)
    dev.off()
    
  }
  
}
