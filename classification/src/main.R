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

library(scatterplot3d)
library(rpart.plot)
library(corrplot)
library(ggplot2)
library(GGally)
library(rpart)
library(dplyr)
library(caret)
library(ISLR)
library(pROC)
library(rgl)

## ---------------------------

## load up our functions into memory

source("src/plot_config.R")
source("src/outliers.R")

## ---------------------------

# Set variables ========================

# set.seed(10)
set.seed(451)
clean_data <- FALSE
plot_data <- TRUE
plot_heavy <- FALSE
plot_3d <- FALSE


# Load data ============================

df <- read.table("data/raw/data.csv", header = T, sep = ",")

df$diagnosis <- as.factor(df$diagnosis)
df$data <- df[,-1]
df.features <- df[, c(3:32)]

if (plot_data) {
  boxplotF("Features", df.features)
}


# Data analisys ========================

if (plot_data) {
  analisys <- df$data %>%
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
      df$data,
      columns = 2:11,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Mean"
    )
    
    # Se group
    ggpairs(
      df$data,
      columns = 12:21,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Se"
    )
    
    # Worst group
    ggpairs(
      df$data,
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
    df$data = remove_outliers(df$data, i)
  }
  boxplotF("No Outliers", df$data[,-1])
}


# Plot corrplot ========================
# Correlation between features
if (plot_data) {
  corrplot(
    title = "Features correlation",
    cor(df$data[, -1]),
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
         nrow(df$data),
         replace = TRUE,
         prob = c(0.8, 0.2))
train_set <- df$data[sample,]
test_set <- df$data[!sample,]


# Feature selection =====================

pr_comp <- prcomp(train_set[, -1], scale = TRUE, center = TRUE)

std_dev <- pr_comp$sdev
pr_var <- std_dev ^ 2
prop_varex <- pr_var / sum(pr_var)
sum(prop_varex[1:10])
# 10 Principal Components explain about 95.4 % of total variance.
# So by using PCA, the dimensions are reduces from 30 to 10.

# New training set with 10 principal components
train_set$pca <-
  data.frame(diagnosis = train_set$diagnosis, pr_comp$x) [, 1:11]


# Plot results --------------------------
if (plot_data) {
  ## Corr plot
  show(ggcorr(
    cbind(train_set[, -1], pr_comp$x[, c(1:10)]),
    label = TRUE,
    cex = 2.5
  ))
  
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
  scatterplot3d(pr_comp$x[, 1:3],
                pch = 16,
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


# Classification Tree ===================

model_tree <-
  rpart(diagnosis ~ .,
        data = train_set$pca,
        method = "class")


#Plotting best size of tree -> on minimum error
minimum.error <- which.min(model_tree$cptable[, "xerror"])
optimal.complexity <- model_tree$cptable[minimum.error, "CP"]
if (plot_data) {
  plotcp(model_tree)
  points(minimum.error,
         model_tree$cptable[minimum.error, "xerror"],
         col = "red",
         pch = 19)
}

model_prune <- prune(model_tree, cp = optimal.complexity)

if (plot_data) {
  show(rpart.plot(
    model_tree,
    type = 1,
    extra = 100,
    box.palette = "-RdYlGn",
    branch.lty = 2
  ))
  show(rpart.plot(
    model_prune,
    type = 1,
    extra = 100,
    box.palette = "-RdYlGn",
    branch.lty = 2
  ))
}

train_set$pred <-
  predict(model_prune, newdata = train_set$pca, type = "class")

print("=== CLASSIFICATION TREE (train) ===")
show(
  confusionMatrix(
    data = train_set$pred,
    reference = train_set$pca$diagnosis,
    positive = "M"
  )
)


# Test ==================================
test_prcomp <- prcomp(test_set[, -1], scale = TRUE, center = TRUE)
test_set$pca <-
  data.frame(diagnosis = test_set$diagnosis, test_prcomp$x) [, 1:11]


test_set$pred <-
  predict(model_prune, newdata = test_set$pca, type = "class")

print("=== CLASSIFICATION TREE (test) ===")
show(
  confusionMatrix(
    data = test_set$pred,
    reference = test_set$pca$diagnosis,
    positive = "M"
  )
)

probs <-
  predict(model_prune, newdata = test_set$pca, type = "prob")[, 1]

# ROC
if (plot_data) {
  show(
    roc(
      response = (test_set$pca$diagnosis == "M"),
      predictor = probs,
      auc = TRUE,
      ci = TRUE,
      plot = TRUE,
      main = "Curva ROC",
      legacy.axes = TRUE
    )
  )
}


# K-fold Cross Validation ================

pr_comp <- prcomp(df$data[, -1], scale = TRUE, center = TRUE)
df$pca <-
  data.frame(diagnosis = df$data$diagnosis, pr_comp$x) [, 1:11]


# Create a trainControl object to control how the train function creates the model
train_control <-
  trainControl(method = "cv",
               number = 5)

# Set required parameters for the model type we are using**
tune_grid = expand.grid(cp = c(0.001))


# Use the train() function to create the model
validated_tree <-
  train(
    diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
    data = df$pca,
    # Data set
    method = "rpart",
    # Model type(decision tree)
    trControl = train_control,
    
  )

validated_tree         # View a summary of the model
trellis.par.set(caretTheme())
show(
  rpart.plot(
    validated_tree$finalModel,
    type = 1,
    extra = 100,
    box.palette = "-RdYlGn",
    branch.lty = 2
  )
)

validated_tree$pred <-
  predict(validated_tree$finalModel,
          newdata = test_set$pca,
          type = "class")
print("=== CLASSIFICATION TREE (k-fold) ===")

show(
  confusionMatrix(
    data = validated_tree$pred,
    reference = test_set$pca$diagnosis,
    positive = "M"
  )
)

validated_tree$prob <-
  predict(validated_tree$finalModel,
          newdata = test_set$pca,
          type = "prob")
