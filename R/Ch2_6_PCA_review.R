
### Library
library(tidyverse)
library(here)
library(PCDimension)
library(factoextra)
library(ggsci)

### Import data
data_owl <- data_clean_1

data_owl_vars <- data_owl %>%
  select(3:32)

### PCA
pca <- princomp(data_owl_vars, cor = TRUE, scores = TRUE)

str(pca)

summary(pca, loadings = TRUE, cutoff = 0)

screeplot(pca)

brokenStick(1, 30)
brokenStick(2, 30)
brokenStick(3, 30)
brokenStick(4, 30)

# correlation between PCs and x variables
Xscaled <- scale(data_owl_vars, center = TRUE, scale = TRUE)
variables_cor <- round(cor(pca$scores, Xscaled,),3) %>%
  replace(., . <= 0.6, NA) 

variables_cor[1:4, ]

# visualization
pca_var <- fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

ggsave(filename = here("docs", "figures", "review_PCA_var.jpg"),
       plot = pca_var,
       height = 15,
       width = 15,
       units = "cm",
       dpi = 300)


groups <- data_owl %>%
  mutate(group = str_split_i(individual, pattern = "_", i = 3)) %>%
  pull(group) %>%
  as.factor()

pca_ind <- fviz_pca_ind(pca,
             col.ind = groups, # color by groups
             geom = "point",
             palette = pal_jco()(10),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             ellipse.level = 0.99,
             legend.title = "site",
             repel = TRUE)

ggsave(filename = here("docs", "figures", "review_PCA_ind.jpg"),
       plot = pca_ind,
       height = 15,
       width = 15,
       units = "cm",
       dpi = 300)



### LDA table

model

model$prior

model$scaling

write_csv(model$scaling %>% as_tibble(), here("docs", "tables", "LDA_table.csv"))
