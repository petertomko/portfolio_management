# Portfolio Management

## Backtesting

- fit model on admissible stocks (from ADF test and Hurst Exponent)

- build/re-create Patria cost schema

- optimize weights of stocks in portfolio

- monitor performance

- re-balance every month, i.e. in period from 1.1.2018 - 31.12.2020

- compare to simple benchmark model (get last month performance based on which the optimization is performed)

- architecture
  - **./tmp** -> storage of models, stored as **.RData**
  - **./tmp/estimated ... model - tmp.RData** -> contains **modelling_df** object
  - **modelling_df** -> contains columns *ticker*, *price_type*, *data*, *year*, *month*, *created_at*, *train_df*, *test_df*, *model*, 
*train_eval_df*, *test_eval_df*

## Portfolio Manager v1

App that offers simple functionality for managing portfolio

- collecting info about buys (date, price)

- monitoring actual performance of portfolio

- visualize backtesting performance

- initialize ETL

- create recommendation for upcoming month (run prediction model, run optimization, create weights)
