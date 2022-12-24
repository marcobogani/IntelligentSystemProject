## ---------------------------
##
## Script name: main.R
##
## Purpose of script: Breast Cancer Classification
##
## Authors: Marco Bogani, Omar Tornaghi
##
## Date Created: 2022-12-23
##
## Copyright (c) Marco Bogani - Omar Tornaghi, 2022
## Email: mbogani@studenti.uninsubria.it
## Email: otornaghi@studenti.uninsubria.it
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## ---------------------------
## this is needed on some PCs to increase memory allowance,
## but has no impact on macs.
# memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(InformationValue)
library(scatterplot3d)
library(rpart.plot)
library(corrplot)
library(ggplot2)
library(GGally)
library(rpart)
library(dplyr)
library(caret)
library(ISLR)
library(rgl)

## ---------------------------

## load up our functions into memory

source("src/plot_config.R")
source("src/outliers.R")

## ---------------------------

# Set variables ========================

set.seed(10)
clean_data <- FALSE
plot_data <- TRUE
plot_heavy <- FALSE
plot_3d <- FALSE


# Load data ============================

df <- read.table("data/raw/data.csv", header = T, sep = ",")

df$diagnosis <- as.factor(df$diagnosis)
df.data <- df[,-1]
df.features <- df[, c(3:32)]

if (plot_data) {
  boxplotF("Features", df.features)
}


# Data analisys ========================

if (plot_data) {
  analisys <- df.data %>%
    group_by(diagnosis) %>% # Variable to be transformed
    count() %>%
    ungroup() %>%
    mutate(perc = `n` / sum(`n`)) %>%
    arrange(perc) %>%
    mutate(labels = scales::percent(perc))
  
  show(
    ggplot(analisys, aes(
      x = "", y = perc, fill = diagnosis
    )) +
      ggtitle("Diagnosis") +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        line = element_blank(),
      ) +
      geom_col() +
      geom_label(
        aes(label = labels),
        position = position_stack(vjust = 0.5),
        show.legend = FALSE
      ) +
      coord_polar(theta = "y")
  )
  
  if (plot_heavy) {
    # Mean group
    ggpairs(
      df.data,
      columns = 2:11,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Mean"
    )
    
    # Se group
    ggpairs(
      df.data,
      columns = 12:21,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Se"
    )
    
    # Worst group
    ggpairs(
      df.data,
      columns = 22:31,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Worst"
    )
  }
}


# Clean data ===========================

if (clean_data) {
  for (i in c("area_worst", "area_mean", "area_se", "perimeter_worst")) {
    df.data = remove_outliers(df.data, i)
  }
  boxplotF("No Outliers", df.data[,-1])
}


# Plot corrplot ========================
# Correlation between features
if (plot_data) {
  corrplot(
    title = "Features correlation",
    cor(df.features),
    diag = FALSE,
    method = "square",
    order = "FPC",
    tl.srt = 90,
    tl.cex = 0.6,
  )
}


# Data set split ========================

sample <-
  sample(c(TRUE, FALSE),
         nrow(df.data),
         replace = TRUE,
         prob = c(0.7, 0.3))
train_set <- df.data[sample,]
test_set <- df.data[!sample,]


# Feature selection =====================

pr_comp <- prcomp(train_set[, -1], scale = TRUE, center = TRUE)

std_dev <- pr_comp$sdev
pr_var <- std_dev ^ 2
prop_varex <- pr_var / sum(pr_var)
sum(prop_varex[1:10])
# 10 Principal Components explain about 95.4 % of total variance.
# So by using PCA, the dimensions are reduces from 30 to 10.


# Plot results --------------------------
if (plot_data) {
  ## Corr plot
  ggcorr(cbind(train_set[, -1], pr_comp$x[, c(1:10)]),
         label = TRUE,
         cex = 2.5)
  
  ## Scree plot
  plot(pr_comp, type = "lines", main = "Scree plot (PCA-prcomp)")
  title(xlab = "PC")
  
  ## Cumulative Proportion of Variance Explained
  plot(cumsum(prop_varex),
       xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
  abline(h = sum(prop_varex[1:10]),
         col = 'red',
         v = 10)
  
  ## Scatter plot (PC1, PC2)
  show(ggplot(
    as.data.frame(pr_comp$x),
    aes(
      x = PC1,
      y = PC2,
      col = train_set$diagnosis
    )
  ) + geom_point(alpha = 0.5))
  
  ## Biplot (PC1, PC2)
  biplot(pr_comp,
         cex = c(.7, .7),
         col = c("gray", "red"))
  
  ## 3D scatter plot (PC1, PC2, PC3)
  scatterplot3d(pr_comp$x[, 1:3], pch = 16,
                color = as.numeric(train_set$diagnosis))
}

if (plot_3d) {
  plot3d(
    pr_comp$x[, 1:3],
    col = as.integer(train_set$diagnosis),
    type = "s",
    size = 0.5
  )
  play3d(spin3d(axis = c(1, 1, 1), rpm = 5), duration = 10)
}


# Model Building ========================

# New training set with 10 principal components
train_set.pca <- data.frame(diagnosis = train_set$diagnosis, pr_comp$x) [,1:11]

model_tree <-
  rpart(diagnosis ~ .,
        data = train_set.pca,
        method = "anova")

#transform test into PCA
test_set.pca <- predict(pr_comp, newdata = test_set)
test_set.pca <- as.data.frame(test_set.pca)[,1:11]

pred_mt <- predict(model_tree, test_set.pca)
confusionMatrix(pred_mt, test_set$diagnosis, positive = "M")


#Plotting best size of tree -> on minimum error
plotcp(model_tree)
minimum.error <- which.min(model_tree$cptable[, "xerror"])
optimal.complexity <- model_tree$cptable[minimum.error, "CP"]
points(minimum.error, model_tree$cptable[minimum.error, "xerror"],
       col = "red", pch = 19)

model_prune <- prune(model_tree, cp = optimal.complexity)

rpart.plot(model_tree, type=1, extra=100, box.palette ="-RdYlGn", branch.lty = 2)
rpart.plot(model_prune, type=1, extra=100, box.palette ="-RdYlGn", branch.lty = 2)




