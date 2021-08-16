library("prophet")

# ----- Estimate Prophet Model -----
ProphetModel <- R6::R6Class(
  "ProphetModel",
  
  public = list(
    
    regressors = NULL,
    model_df = NULL,
    fitted_model = NULL,
    
    add_variables = function(vars_list) {
      
      self$regressors = vars_list
      
    },
    
    fit = function() {
      
    },
    
    transform = function() {
      
    },
    
    fit_transform = function() {
      
    }
    
  )
)