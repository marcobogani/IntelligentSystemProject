#-------------------------------------------------------
#---       Università degli Studi dell'Insubria      ---
#--- Title  : Classify Breast Cancer                 ---
#--- Authors: Bogani Marco, Tornaghi Omar            ---
#-------------------------------------------------------

library(corrplot)
library(ggplot2)
library(GGally)
library(rgl)

source("src/plot_config.R")
source("src/outliers.R")

# Set variables ========================

set.seed(10)
clean_data <- FALSE
plot_data <- TRUE


# Load data ============================

df <- read.table("data/raw/data.csv", header = T, sep = ",")

# Feature + dependent variable (diagnosis)
df.data <- df[, -1]
# Only features (indipendent variables)
df.features <- df[, c(3:32)]

if (plot_data) {
  boxplotF("Features", df.features)
}


# Clean data ===========================
if (clean_data) {
  for (i in c("area_worst", "area_mean", "area_se", "perimeter_worst")) {
    df.features = remove_outliers(df.features, i)
  }
  boxplotF("No Outliers", df.features)
}


# Plot corrplot ========================
# Correlation between variables
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
plot(df.pca, type = "lines")

ggplot(as.data.frame(df.pca$x),
       aes(
         x = PC1,
         y = PC2,
         col = df.data$diagnosis
       )) + geom_point(alpha = 0.5)

biplot(df.pca, cex = c(.7, .7), col = c("gray", "red"))
