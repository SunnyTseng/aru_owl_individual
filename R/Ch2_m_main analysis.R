###
### Name: main analysis
### 
### Author: Sunny Tseng
### Date: 2022-10-18
###


###
### Library
###
library(here)
library(tidyverse)
library(factoextra)
library(ggbiplot)
library(corrplot)
library(psych)
library(caret)
library(MASS)

# avoid name overwrite
here <- here::here
select <- dplyr::select
filter <- dplyr::filter
summarize <- dplyr::summarize

###
### Data import and cleaning
###
data <- read_csv(here("Ch2_owl_individual", "data", "barred_owl_recordings_list_parameters_2.csv"))

data_clean <- data %>%
  filter_all(all_vars(. != 0)) %>%
  mutate(site = str_sub(sound.files, start = 1, end = 5),
         year = str_sub(sound.files, start = 7, end = 10),
         month = str_sub(sound.files, start = 11, end = 12),
         day = str_sub(sound.files, start = 13, end = 14),
         unique_ID = seq(from = 1, to = nrow(.)),
         individual = paste0(year, "_", site))

sites <- data_clean %>%
  group_by(site) %>%
  summarize(n = n()) %>%
  filter(n >= 9) %>%
  pull(site)

data_clean_1 <- data_clean %>%
  filter(site %in% sites)
  
###
### Data visualization
###




###
### Statistics
###
# examine the variables performance by looking at the CV
statistics <- data_clean_1 %>%
  select(site, sl:meandom_8) %>%
  pivot_longer(cols = c(-site), names_to = "variable", values_to = "value") %>%
  group_nest(variable) %>%
  mutate(mean = map_dbl(.x = data, .f =~ .x %>% pull(value) %>% mean()),
         sd = map_dbl(.x = data, .f =~ .x %>% pull(value) %>% sd()),
         min = map_dbl(.x = data, .f =~ .x %>% pull(value) %>% min()),
         max = map_dbl(.x = data, .f =~ .x %>% pull(value) %>% max()),
         CV_group = map_dbl(.x = data, .f =~ sd(.x$value)/mean(.x$value)*100),
         CV_individual = map_dbl(.x = data, .f =~ .x %>% 
                                   group_by(site) %>%
                                   dplyr::summarise(CV = sd(value)/mean(value)) %>%
                                   pull(CV) %>%
                                   mean() *100),
         ANOVA = map_dbl(.x = data, .f =~ .x %>%
                           lm(formula = value ~ site) %>%
                           anova() %>%
                           .$`Pr(>F)` %>%
                           .[1])) %>%
  select(-data)
# write_csv(statistics, here("Ch2_owl_individual", "docs", "features_statistics.csv"))


# examine the variable correlation
cor <- data_clean %>%
  dplyr::select(sl:meandom_8) %>%
  cor() %>%
  corrplot(method = "color") 


###
### Hypothesis test
###
# Kaiser-Meyer-Oklin Test, sampling adequacy predicts if data are likely to factor well, 
# based on corelation and partial correlation. Measured Sampling Adequacy (MSA) should be
# larger than 0.6. Here we have 0.89, perfect!
test_KMO <- data_clean_1 %>%
  dplyr::select(sl:meandom_1) %>%
  cor() %>%
  KMO()

# Bartlett's Test, allows you to testing whether a correlation matrix is factorable 
# (i.e., the correlations differ from 0). If significant, then some variables are correlated
# to each other, which is required for factor analysis. Here we have p-value = 0.74, which 
# means there is no significant difference between the corvariance matrix to an identity matrix
test_Bartlett <- data_clean_1 %>%
  dplyr::select(sl:duration_1, dfrange_1, meandom_1) %>%
  cor() %>%
  cortest.bartlett(n = nrow(.), diag = TRUE)

# Determinant, a formula about multicollinearity. The result should preferably fall below .00001.
# The value of our result is 1.167361e-10, perfect!
test_det <- data_clean_1 %>%
  select(sl:meandom_1) %>%
  cor() %>%
  det()


###
### Main analysis 
###
set.seed(100)

data_group <- data_clean_1 %>%
  mutate(group = sample(1:5, nrow(.), replace = TRUE))

individuals <- data_clean_1 %>%
  pull(individual) %>%
  unique()

table_all <- matrix(0, nrow = length(individuals), ncol = length(individuals))
for (i in 1:5) {
  # split training data and test data
  data_train <- data_group %>%
    filter(group != i)
  data_test <- data_group %>%
    filter(group == i)
  
  # data pre-processing
  preproc.param <- data_train %>% 
    preProcess(method = c("center", "scale"))
  train_transformed <- preproc.param %>% predict(data_train)
  test_transformed <- preproc.param %>% predict(data_test)
  
  # model fitting
  model <- train_transformed %>%
    select(individual, sl, i2, i3, duration_8, meandom_6, meandom_1,
           meandom_8, duration_3, duration_2) %>%
    lda(individual ~ ., data = .)
  
  # model evaluation
  predictions <- model %>% predict(test_transformed)
  
  observed <- test_transformed$individual %>%
    as_factor()
  levels(observed) <- individuals
  
  predicted <- predictions$class %>%
    as_factor()
  levels(predicted) <- individuals
  
  table_i <- table(predicted, observed)
  table_all <- table_all + table_i
}
confusionMatrix(table_all)$overall[1]
confusionMatrix(table_all)$overall[2]

# variable selection
train_transformed %>%
  greedy.wilks(individual ~ sl + i1 + i2 + i3 + min_d + max_d + 
                 duration_1 + duration_2 + duration_3 + duration_4 + 
                 duration_5 + duration_6 + duration_7 + duration_8 +
                 dfrange_1 + dfrange_2 + dfrange_3 + dfrange_4 +
                 dfrange_5 + dfrange_6 + dfrange_7 + dfrange_8 +
                 meandom_1 + meandom_2 + meandom_3 + meandom_4 +
                 meandom_5 + meandom_6 + meandom_7 + meandom_8
               , data = .)


# visualization
lda_data <- cbind(train_transformed, predict(model)$x)
ggplot(lda_data, aes(LD1, LD2)) +
  geom_point(aes(color = individual))

ggord(model, data_train$site, arrow = 0)


###
### Figures & Tables
###

singing_day <- data_clean_1 %>%
  select(sound.files, song, site, year, month, day) %>%
  mutate(date = ymd(paste0(year, month, day)),
         julian = yday(date)) %>%
  group_by(site, julian) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = julian, value = n)






###
### Other codes
###

### data visualization 
boxplots <- data_clean %>%
  pivot_longer(cols = sl:meandom_8, names_to = "feature", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(x = site, y = value)) + 
  facet_grid(.~ feature, scales='free')

comparison_t <- NULL
for (i in 3:32) {
  var <- names(data_clean)[i]
  test <- t.test(data_clean %>% pull(i) ~ data_clean$site, var.equal = TRUE)
  t_value <- test$statistic
  p_value <- test$p.value
  
  temp <- tibble(var = var, t_value = t_value, p_value = p_value)
  comparison_t <- rbind(comparison_t, temp)
}
comparison_t <- comparison_t %>%
  mutate(significance = if_else(p_value <= 0.05, T, F))



### DFA code online
training_samples <- data_clean$song %>%
  createDataPartition(p = 0.8, list = FALSE)

train_data <- data_clean[training_samples, ]
test_data <- data_clean[-training_samples, ]

# Estimate preprocessing parameters
preproc.param <- train_data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train_transformed <- preproc.param %>% predict(train_data)
test_transformed <- preproc.param %>% predict(test_data)


model <- train_transformed %>%
  select(individual, sl:meandom_1) %>%
  lda(individual ~., data = .)
# Make predictions
predictions <- model %>% predict(test_transformed)
# Model accuracy
mean(predictions$class == test_transformed$individual)

test <- table(test_transformed$individual, predictions$class)

write_csv(test %>% as_tibble(), "confusion_matrix.csv")

write.table(test, 'confusion_matrix.txt')

# Plot the result
lda_data <- cbind(train_transformed, predict(model)$x)
ggplot(lda_data, aes(LD1, LD2)) +
  geom_point(aes(color = individual))


### PCA online

# Both prcomp and princomp are available and provide similar results. Here we use prcomp based on the 
# suggestion in https://stats.stackexchange.com/questions/20101/what-is-the-difference-between-r-functions-prcomp-and-princomp
owl_pca <- data_clean %>%
  select(sl:meandom_8) %>%
  prcomp(scale = TRUE)

pca_values <- fviz_eig(owl_pca)

fviz_pca_ind(owl_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )

fviz_pca_ind(owl_pca,
             col.ind = data_clean$site, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
             )

fviz_pca_var(owl_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

pca_plot <- ggbiplot(owl_pca,
         ellipse=TRUE, 
         groups=data_clean$site)





