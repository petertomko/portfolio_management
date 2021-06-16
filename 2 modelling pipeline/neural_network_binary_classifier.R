# ------ Load Library -----
library("tidyverse")
library("keras")
library("fastDummies")
library("caret")
library("tensorflow")
library("kerasR")
library("reticulate")

TRAIN_DT <- as.Date("2021-01-01")

if(as.character(Sys.info()["nodename"]) == "ND0151MB"){
  board_nm <- "Portfolio Management"
}

if(as.character(Sys.info()["nodename"]) == "copernicus64gb"){
  board_nm <- "Portfolio Management (Copernicus)"
}

if(!(as.character(Sys.info()["nodename"]) %in% c("copernicus64gb", "ND0151MB"))){
  board_nm <- "Portfolio Management"
}

# ----- Register Board -----
pins::board_register_local(name = board_nm)

# ----- Load Data -----
modelling_data <- 
  
  # - Download Data
  pins::pin_get("modelling_data", board_nm) %>% 
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
  dplyr::select(-dt, -ticker, 
                -open, -high, -low, -close, -p_adjusted, -volume) %>% 
  as.data.frame()

train_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Train") %>% 
  dplyr::select(-sample_type) %>% 
  as.data.frame()

test_df <- 
  modelling_data %>% 
  dplyr::filter(sample_type %in% "Test") %>% 
  dplyr::select(-sample_type) %>% 
  as.data.frame()

# - Size and format of data frame
X_train <- train_df %>% 
  dplyr::select(-target_10w) %>% 
  scale()
y_train <- keras::to_categorical(train_df$target_10w)

X_test <- test_df %>% 
  scale()
y_test <- keras::to_categorical(test_df$target_10w)

# Network design
binclass_model <- keras_model_sequential()
binclass_model %>%
  
  # Input layer
  layer_dense(units = 32, activation = "relu", input_shape =  ncol(X_train)) %>% 
  layer_dropout(rate = 0.1) %>% 
  
  # Hidden layer
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(units = 2, activation = "sigmoid")

# Network config
history <- 
  binclass_model %>% 
  compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = c("accuracy")
  )

# Running our data
binclass_model %>% fit(
  X_train, y_train, 
  epochs = 20, 
  batch_size = 1000,
  validation_split = 0.2
)
summary(binclass_model)

# Calculating accuracy
test_pred <- predict_proba(binclass_model, x = X_test)

evaluation <- 
  binclass_model %>% evaluate(
    x = X_test,
    y = y_test,
    batch_size = 1000
  )

# Confusion Matrix
table(factor(predictions, 
             levels = min(test_df$target_10w):max(test_df$target_10w)),
      
      factor(test_df$target_10w, 
             levels = (test_df$target_10w):max(test_df$target_10w)))
