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
## this is needed on some PCs to increase memory allowance, but has no impact on macs.
# memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(scatterplot3d)
library(corrplot)
library(ggplot2)
library(GGally)
library(dplyr)
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

# Feature + dependent variable (diagnosis)
df.data <- df[, -1]
# Only features (indipendent variables)
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
    df.features = remove_outliers(df.features, i)
  }
  boxplotF("No Outliers", df.features)
}


# Plot corrplot ========================
# Correlation between features
corrplot(
  title = "Features correlation",
  cor(df.features),
  diag = FALSE,
  method = "color",
  order = "FPC",
  tl.srt = 90,
  tl.cex = 0.6,
)


# Feature selection =====================

# PC weight
df.pca <- prcomp(df.features, scale = TRUE, center = TRUE)

if (plot_data) {
  ## Corr plot
  ggcorr(cbind(df.features, df.pca$x[,c(1:10)]), label = TRUE, cex = 2.5)
  
  ## Scree plot
  plot(df.pca, type = "lines", main = "Scree plot (PCA-prcomp)")
  title(xlab = "PC")
  
  ## Scatter plot (PC1, PC2)
  show(ggplot(
    as.data.frame(df.pca$x),
    aes(
      x = PC1,
      y = PC2,
      col = df.data$diagnosis
    )
  ) + geom_point(alpha = 0.5))
  
  ## Biplot (PC1, PC2)
  biplot(df.pca,
         cex = c(.7, .7),
         col = c("gray", "red"))
  
  ## 3D scatter plot
  scatterplot3d(df.pca$x[, 1:3], pch = 16,
                color = as.numeric(as.factor(df$diagnosis)))
}

if (plot_3d) {
  plot3d(
    df.pca$x[, 1:3],
    col = as.integer(as.factor(df$diagnosis)),
    type = "s",
    size = 0.5
  )
  play3d(spin3d(axis = c(1, 1, 1), rpm = 5), duration = 10)
}


