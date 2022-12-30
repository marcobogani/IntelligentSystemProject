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
exclude_fault <- TRUE

# Load data ============================

df <- read.table("data/raw/data.csv", header = T, sep = ",")

df$diagnosis <- as.factor(df$diagnosis)
df <- df[, -1]

if (plot_data) {
  boxplotF("Features", df[,-1])
}


# Data analisys ========================

if (plot_data) {
  analisys <- df %>%
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
      df,
      columns = 2:11,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Mean"
    )
    
    # Se group
    ggpairs(
      df,
      columns = 12:21,
      mapping = aes(color = as.factor(df$diagnosis)),
      upper = list(continuous = wrap("cor", size = 6)),
      title = "Scatterplot Matrix for Se"
    )
    
    # Worst group
    ggpairs(
      df,
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
    df = remove_outliers(df, i)
  }
  boxplotF("No Outliers", df[, -1])
}


# Plot corrplot ========================
# Correlation between features
if (plot_data) {
  corrplot(
    title = "Features correlation",
    cor(df[,-1]),
    diag = FALSE,
    type = "upper",
    method = "circle",
    order = "FPC",
    tl.srt = 90,
    tl.cex = 0.6,
  )
}


# Data set split ========================

sample <-
  sample(c(TRUE, FALSE),
         nrow(df),
         replace = TRUE,
         prob = c(0.8, 0.2))
train_set <- df[sample, ]
test_set <- df[!sample, ]


# Feature selection =====================

train_set.pc <- prcomp(train_set[,-1], scale = TRUE, center = TRUE)

std_dev <- train_set.pc$sdev
pr_var <- std_dev ^ 2
prop_varex <- pr_var / sum(pr_var)
sum(prop_varex[1:10])
# 10 Principal Components explain about 95.4 % of total variance.
# So by using PCA, the dimensions are reduces from 30 to 10.

# New training set with 10 principal components
train_set.pca <-
  data.frame(diagnosis = train_set$diagnosis, train_set.pc$x) [, 1:11]

# Test set
test_set.pc <- prcomp(test_set[,-1], scale = TRUE, center = TRUE)
test_set.pca <-
  data.frame(diagnosis = test_set$diagnosis, test_set.pc$x) [, 1:11]


# Plot results --------------------------
if (plot_data) {
  ## Corr plot
  show(ggcorr(
    cbind(train_set[,-1], train_set.pc$x[, c(1:10)]),
    label = TRUE,
    cex = 2.5
  ))
  
  ## Scree plot
  plot(train_set.pc, type = "lines", main = "Scree plot (PCA-prcomp)")
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
    as.data.frame(train_set.pc$x),
    aes(
      x = PC1,
      y = PC2,
      col = train_set$diagnosis
    )
  ) + geom_point(alpha = 0.5))
  
  ## Biplot (PC1, PC2)
  biplot(train_set.pc,
         cex = c(.7, .7),
         col = c("gray", "red"))
  
  ## 3D scatter plot (PC1, PC2, PC3)
  scatterplot3d(train_set.pc$x[, 1:3],
                pch = 16,
                color = as.numeric(train_set$diagnosis))
}

if (plot_3d) {
  plot3d(
    train_set.pc$x[, 1:3],
    col = as.integer(train_set$diagnosis),
    type = "s",
    size = 0.5
  )
  play3d(spin3d(axis = c(1, 1, 1), rpm = 5), duration = 10)
}


# Classification Tree ===================

model_tree <-
  rpart(diagnosis ~ .,
        data = train_set.pca,
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
  # Initial model
  show(rpart.plot(
    model_tree,
    type = 1,
    extra = 100,
    box.palette = "-RdYlGn",
    branch.lty = 2
  ))
  
  # Pruned model
  show(rpart.plot(
    model_prune,
    type = 1,
    extra = 100,
    box.palette = "-RdYlGn",
    branch.lty = 2
  ))
}

train_set$pred <-
  predict(model_prune, newdata = train_set.pca, type = "class")

print("=== CLASSIFICATION TREE (train) ===")
show(
  confusionMatrix(
    data = train_set$pred,
    reference = train_set.pca$diagnosis,
    mode = "everything",
    positive = "M"
  )
)


# Test ==================================
test_set$pred <-
  predict(model_prune, newdata = test_set.pca, type = "class")

print("=== CLASSIFICATION TREE (test) ===")
show(
  confusionMatrix(
    data = test_set$pred,
    reference = test_set.pca$diagnosis,
    mode = "everything",
    positive = "M"
  )
)


# K-fold Cross Validation (rpart) ========

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
    data = train_set.pca,
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
          newdata = test_set.pca,
          type = "class")
print("=== CLASSIFICATION TREE (k-fold) ===")

show(
  confusionMatrix(
    data = validated_tree$pred,
    reference = test_set.pca$diagnosis,
    mode = "everything",
    positive = "M"
  )
)

validated_tree$prob <-
  predict(validated_tree$finalModel,
          newdata = test_set.pca,
          type = "prob")


# Logistic Regression ====================
if (!exclude_fault) {
  lr_model <-
    glm(
      diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
      data = train_set.pca,
      family = binomial(link = "logit")
    )
  summary(lr_model)
  
  train_set$pred <-
    predict(lr_model, newdata = train_set.pca)
  
  test_set$pred <-
    predict(lr_model, newdata = test_set.pca)
  
  confusionMatrix(
    data = test_set$pred,
    reference = test_set.pca$diagnosis,
    mode = "everything",
    positive = "M"
  )
}


# K-NN ===================================

# Find K that minimize error
kmax <- 30
test_error <- numeric(kmax)
for (k in 1:kmax) {
  knn_pred <-
    as.factor(
      knn3Train(
        train_set.pca[,-1],
        test_set.pca[,-1],
        train_set.pca[, 1],
        k = k,
        prob = FALSE,
        use.all = FALSE
      )
    )
  cm <-
    confusionMatrix(data = knn_pred,
                    reference = test_set.pca[, 1],
                    mode = "everything",
                    positive = "M")
  
  test_error[k] <- 1 - cm$overall[1]
}

k_min <- which.min(test_error)
knn_pred <-
  as.factor(
    knn3Train(
      train_set.pca[,-1],
      test_set.pca[,-1],
      train_set.pca[, 1],
      k = k_min,
      prob = FALSE,
      use.all = FALSE
    )
  )

# Cross Validation
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit <- train(
  diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
  method     = "knn",
  tuneGrid   = expand.grid(k = 1:kmax),
  trControl  = trControl,
  metric     = "Accuracy",
  data       = train_set.pca
)

# Test ==================================
print("========== K-NN (test) ==========")
cm <-
  confusionMatrix(data = knn_pred,
                  reference = test_set.pca[, 1],
                  mode = "everything",
                  positive = "M")
show(cm)

test_set$pred <-
  predict(fit, newdata = test_set.pca)

print("======= K-NN (test - cv) =======")
show(
  confusionMatrix(
    data = test_set$pred,
    reference = test_set.pca$diagnosis,
    mode = "everything",
    positive = "M"
  )
)

# Plot K-NN ==============================

if (plot_data) {
  ggp <-
    ggplot(data.frame(test_error), aes(x = 1:kmax, y = test_error)) +
    geom_line(colour = "blue") +
    geom_point(colour = "blue") +
    xlab("K value") + ylab("Test error") +
    ggtitle(paste0(
      "Optimal K value = ",
      k_min,
      " (min error = ",
      format((1 - cm$overall[1]) * 100, digits = 4),
      "%)"
    ))
  print(ggp)
  
  # Cross Validation
  show(plot(fit))
  
  # ROC
  probs <-
    predict(fit, newdata = test_set.pca, type = "prob")[, 1]
  pROC_obj <- roc(
    response = (test_set.pca$diagnosis == "M"),
    predictor = probs,
    smoothed = TRUE,
    ci = TRUE,
    ci.alpha = 0.9,
    stratified = FALSE,
    plot = TRUE,
    auc.polygon = TRUE,
    max.auc.polygon = TRUE,
    grid = TRUE,
    print.auc = TRUE,
    show.thres = TRUE,
    legacy.axes = TRUE
  )
  sens.ci <- ci.se(pROC_obj)
  plot(sens.ci, type = "shape", col = "lightblue")
  plot(sens.ci, type = "bars")
}


# Random Forest ==========================

trControl <-
  trainControl(
    method = "cv",
    number = 5,
    summaryFunction = twoClassSummary,
    savePredictions = TRUE,
    classProbs = TRUE,
    sampling = "up"
  )
rforest <-
  train(
    diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
    data = train_set.pca,
    method = "rf",
    trControl = trControl,
    metric = "Sens",
    ntree = 500,
    cutoff = c(.73, 1 - .73)
  )

rforest

test_set$pred <- predict(rforest, test_set.pca, na.action = na.pass)
cm_rf <- confusionMatrix(
  data = test_set$pred,
  reference = test_set.pca$diagnosis,
  mode = "everything",
  positive = "M"
)

cm_rf



