###
### Name: lda_n_fold_validation
### 
### Author: Sunny Tseng
### Date: 2023-03-09
### Input: dataset as cleaned dataset, and n for n-fold validation
### Output: a tidy confustion matrix ready for analysis
###


lda_n_fold_validation <- function(dataset, n, m){
  
  individuals <- dataset %>%
    pull(individual) %>%
    unique()
  
  table_final <- matrix(0, nrow = length(individuals), ncol = length(individuals))
  for (bootstrapping in 1:m) {
    set.seed(bootstrapping)
    
    data_group <- dataset %>%
      mutate(group = sample(1:n, nrow(.), replace = TRUE))
    
    table_all <- matrix(0, nrow = length(individuals), ncol = length(individuals))
    for (i in 1:n) {
      # split training data and test data
      data_train <- data_group %>%
        filter(group != i)
      data_test <- data_group %>%
        filter(group == i)
      
      # data pre-processing
      train_transformed <- data_train %>% 
        preProcess(method = c("center", "scale")) %>%
        predict(data_train)
      test_transformed <- data_train %>% 
        preProcess(method = c("center", "scale")) %>%
        predict(data_test)
      
      # model fitting
      model <- train_transformed %>%
        select(individual, 
               sl, i2, i3, duration_8, meandom_6, 
               duration_3, meandom_8, duration_2) %>%
        lda(individual ~ ., data = .)
      
      # model evaluation
      predictions <- model %>% 
        predict(test_transformed)
      
      observed <- test_transformed$individual %>%
        as_factor()
      levels(observed) <- individuals
      
      predicted <- predictions$class %>%
        as_factor()
      levels(observed) <- individuals
      
      table_i <- table(predicted, observed)
      table_all <- table_all + table_i
    }
    table_final <- table_final + table_all
    
  }
  
return(table_final)
}

