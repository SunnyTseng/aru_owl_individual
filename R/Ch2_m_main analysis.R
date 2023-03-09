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
library(lubridate)
library(sf)
library(leaflet)

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
group_by <- dplyr::group_by
rename <- dplyr::rename

###
### Data import and cleaning
###
# the original output from BirdNET
data_raw <- read_csv(here("data", "barred_owl_recordings_list.csv"))

data_raw_clean <- data_raw %>%
  filter(year == 2021) %>%
  drop_na() %>%
  mutate(date = ymd(paste0(year, month, day)),
         hour = str_sub(recording, start = 10, end = 11),
         Julian = yday(date), 
         day_period = cut(Julian, 
                          breaks = c(40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)))

# the song dataset after manual extraction of features
data <- read_csv(here("data", "barred_owl_recordings_list_parameters_2.csv"))

data_clean <- data %>%
  filter_all(all_vars(. != 0)) %>%
  mutate(site = str_sub(sound.files, start = 1, end = 5),
         year = str_sub(sound.files, start = 7, end = 10),
         month = str_sub(sound.files, start = 11, end = 12),
         day = str_sub(sound.files, start = 13, end = 14),
         unique_ID = seq(from = 1, to = nrow(.)),
         individual = paste0(year, "_", site),
         date = ymd(paste0(year, month, day)))

sites <- data_clean %>%
  group_by(site) %>%
  summarize(n = n()) %>%
  filter(n >= 9) %>%
  pull(site)

data_clean_1 <- data_clean %>%
  filter(site %in% sites)

# location of the 66 sites
location <- read_csv(here("data", "JPRF_all_sites.csv"))

location_clean <- location %>%
  rename("site" = Name, "longitude" = X, "latitude" = Y) %>%
  select(site, longitude, latitude) %>%
  mutate(group = str_extract(site, pattern = ".+(?=\\_)"))

  
###
### Data visualization
###

# after filtering "un-valid" recordings (recordings with fewer than 5 detections within it)
temporal_recordings <- data_raw_clean %>%
  group_by(date, site) %>%
  summarize(n_recordings = n()) %>%
  ungroup() %>%
  #complete(date, site, fill = list(n_detections = 0)) %>%
  ggplot(aes(x = date, y = site, fill = n_recordings)) +
    geom_point(shape = 22, size = 5) +
    scale_fill_gradient(low = "mistyrose", high = "red") +
    theme_bw() +
    theme(legend.position = c(0.99, 0.05), 
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          axis.title = element_text(size = 14)) +
    labs(fill = "# of recordings", x = "Date", y = "Site", 
         title = "# of BDOW recordings per site per day")
temporal_recordings 


# after filtering "un-qualified" sites (sites with fewer than 9 standard songs within it)\
temporal_songs <- data_clean_1 %>%
  group_by(date, site) %>%
  summarize(n_songs = n()) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = site, fill = n_songs)) +
    geom_point(shape = 22, size = 7) +
    scale_fill_gradient(low = "mistyrose", high = "red") +
    theme_bw() +
    theme(legend.position = c(0.99, 0.05), 
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          axis.title = element_text(size = 14)) +
    labs(fill = "# of calls", x = "Date", y = "Site",
         title = "# of BDOW calls per site per day")
temporal_songs

# spatial distribution of the ARUs and the songs in each sites that is going to be analyzed
JPRF_sf <- full_join(data_location_clean, 
                  data_clean_1 %>% 
                    group_by(site) %>%
                    summarize(n_songs = n()),
                  by = "site", replace_na) %>%
  replace_na(list(n_songs = 0)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

bbox <- JPRF_sf %>% 
  st_bbox() 

bbox <- c(left = bbox[1], bottom = bbox[2], right = bbox[3], top = bbox[4])
names(bbox) <- c("left", "bottom", "right", "top")

#%>%
#  rename(left = xmin, bottom = ymin, right = xmax, top = ymax)

map <- get_map(location = bbox, maptype = "toner-background")

JPRF_plot <- ggplot() +
  geom_sf(aes(colour = group), shape = 3, stroke = 1.5, size = 3, data = JPRF_sf) +
  geom_sf(aes(size = n_songs), shape = 1, stroke = 2, data = JPRF_sf %>% filter(n_songs != 0)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-124.7, -124.1), ylim = c(54.55, 54.85)) +
  theme_bw()

JPRF_plot
#--- dpi = 320 ---#
# ggsave("nc_dpi_320.png", JPRF_plot, height = 5, width = 7, dpi = 300)


#some example code playing with leavlet
field_sites <- JPRF_sf %>%
  rename("site_name" = site)

leaflet() %>%
  setView(lng = -123.5, lat = 52, zoom = 10) %>%
  addProviderTiles("Stamen") %>%
  addCircleMarkers(
    data = field_sites,
    radius = 5,
    color = "black",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = paste("Site name: ", field_sites$site_name)
  )




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
  train_transformed <- data_train %>% 
    preProcess(method = c("center", "scale")) %>%
    predict(data_train)
  test_transformed <- data_train %>% 
    preProcess(method = c("center", "scale")) %>%
    predict(data_test)
  
  # model fitting
  model <- train_transformed %>%
    select(individual, sl, i2, i3, duration_8, meandom_6, meandom_1, meandom_8, duration_3, duration_2) %>%
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
table_all_cm <- confusionMatrix(table_all)
accuracy <- table_all_cm$overall[1]
kappa <- table_all_cm$overall[2]

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

ggord(model, data_train$site, arrow = NULL, txt = NULL) +
  coord_fixed(ratio = 0.7)


df <- melt(table_all_cm$table)
colnames(df) <- c("x", "y", "value")

ggplot(df, aes(x = x, y = y, fill = value %>% log())) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.1, hjust = 0.2))



## Find the relationship between vocal similarity versus spatial distance between
test <- lda_data %>%
  group_by(individual) %>%
  summarize(LD1_mean = mean(LD1),
            LD2_mean = mean(LD2))

JPRF_sf <- full_join(location_clean, 
                     data_clean_1 %>% 
                       group_by(site) %>%
                       summarize(n_songs = n()),
                     by = "site", replace_na) %>%
  replace_na(list(n_songs = 0))

JPRF_sf_m <- JPRF_sf %>%
  select(longitude, latitude) %>%
  as.matrix()
rownames(JPRF_sf_m) <- JPRF_sf$site

dist(JPRF_sf_m, method="euclidean", diag=TRUE, 
     upper=FALSE)



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


fviz_pca_var(owl_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

pca_plot <- ggbiplot(owl_pca, ellipse = TRUE, 
                     groups = data_clean$site,
                     var.axes = FALSE) +
            theme_bw() 





