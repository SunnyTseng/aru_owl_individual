###
### Name: lda_n_fold_validation
### 
### Author: Sunny Tseng
### Date: 2023-03-09
### Input: dataset as cleaned dataset, and n for n-fold validation
### Output: a tidy confustion matrix ready for analysis
###


lda_n_fold_validation <- function(dataset, n, m, p){
  
  variables <- c("sl", "i2", "i3", "duration_8", "meandom_6", 
                 "duration_3", "meandom_8", "duration_2", "meandom_5", "meandom_1") %>%
    head(p)
  
  individuals <- dataset %>%
    pull(individual) %>%
    unique()
  
  table_final <- matrix(0, nrow = length(individuals), ncol = length(individuals))
  accuracy_final <- c()
  kappa_final <- c()
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
        select(all_of(c("individual", variables))) %>%
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
      table_all <- table_all + table_i # from single iteration
      
      table_all_cm <- table_all %>% 
        confusionMatrix()
      accuracy <- table_all_cm$overall[1]
      kappa <- table_all_cm$overall[2]
    }
    table_final <- table_final + table_all # sum across m interations
    accuracy_final <- c(accuracy_final, accuracy)
    kappa_final <- c(kappa_final, kappa)
  }

  table_final_cm <- (table_final/m) %>%
    confusionMatrix()
  
return(list(table_final_cm, accuracy_final, kappa_final))
}

