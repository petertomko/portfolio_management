library("parallel")
library("doParallel")
library("snow")
library("pROC")
library("caret")

TRAIN_DT <- as.Date("2021-01-01")

# ----- Create Cores for Parallel Processing -----
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores, type = "SOCK")
registerDoParallel(cl)

set.seed(825)

# ----- Register Board -----
pins::board_register_local(name = "Portfolio Management")

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data", "Portfolio Management") %>% 
  as.data.frame() %>% 
  
  # create factor TARGET
  dplyr::mutate(
    target = as.factor(dplyr::if_else(target_10w > 0, "Over", "Under"))
  ) %>% 
  as.data.frame() %>% 
  
  # - split train/test
  dplyr::mutate(
    sample_type = dplyr::if_else(dt < TRAIN_DT, "Train", "Test")
  ) %>% 
  
  # - Remove columns
  dplyr::select(-ticker, 
                -open, -high, -low, -close, -p_adjusted, -volume) %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  as.data.frame()

train_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Train" & 
                  lubridate::month(dt) == 5 & 
                  lubridate::year(dt) == 2020) %>% 
  dplyr::select(-sample_type, -dt) %>% 
  as.data.frame()

test_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Test") %>% 
  dplyr::select(-sample_type) %>% 
  as.data.frame()

# ----- Setting the Grid -----
paramGrid <- expand.grid(nrounds = c(25, 50, 75, 100, 125, 150),
                         max_depth = c(2:10),
                         eta = c(0.01, 0.02, 0.035, 0.05, 0.1),
                         gamma = 0.01,
                         colsample_bytree = 1,
                         min_child_weight = c(1, 2, 5, 7, 10),
                         subsample = c(0.1, 0.25, 0.5, 0.75, 1))

# ----- Training Model -----
fitControl <- trainControl(method = "cv",
                           sampling = "smote",
                           classProbs = T,
                           summaryFunction = twoClassSummary)

cvModelFit <- train(x = train_df %>% 
                      dplyr::select(-target_10w, -target) %>% 
                      as.matrix(), 
                    
                    y = train_df %>% 
                      dplyr::select(target) %>% 
                      as.matrix(), 
                    
                    # model and evaluation metric
                    method = "xgbTree",
                    metric = "ROC",
                    
                    # strata sampling
                    strata = train_df %>% 
                      dplyr::select(target) %>% 
                      as.matrix(),
                    
                    trControl = fitControl, 
                    verbose = T,
                    
                    # tuning the grid
                    tuneGrid = paramGrid, 
                    allowParallel = T)
stopCluster(cl)
