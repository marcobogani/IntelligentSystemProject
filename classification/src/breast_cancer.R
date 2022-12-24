#-------------------------------------------------------
#---       Università degli Studi dell'Insubria      ---
#--- Title  : Classify Breast Cancer                 ---
#--- Authors: Bogani Marco, Tornaghi Omar            ---
#-------------------------------------------------------

install.packages("GGally")
library(corrplot)
library(ggplot2)
library(GGally)
library(rgl)

set.seed(0)

# Formatted boxplot
boxplotF <- function (title, data) {
  ## Adjust some graphical parameters.
  par(mar = c(6.1, 4.1, 4.1, 4.1), # change the margins
      lwd = 2, # increase the line thickness
      cex.axis = 1 # increase default axis label size
  )
  
  ## Draw boxplot with no axes.
  boxplot(data, main = title, xaxt = "n", yaxt = "n")
  
  ## Draw x-axis without labels.
  axis(side = 1, labels = FALSE)
  
  ## Draw y-axis.
  axis(side = 2,
       ## Rotate labels perpendicular to y-axis.
       las = 2,
       ## Adjust y-axis label positions.
       mgp = c(3, 0.75, 0))
  
  ## Draw the x-axis labels.
  text(x = 1:length(data),
       ## Move labels to just below bottom of chart.
       y = par("usr")[3] - 0.45,
       ## Use names from the data list.
       labels = names(data),
       ## Change the clipping region.
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 45,
       ## Adjust the labels to almost 100% right-justified.
       adj = 1.1,
       ## Increase label size.
       cex = 1)
}

#-----------------------#
#     Loading data      #
#-----------------------#
df <-
  read.table(
    "C:\\Users\\marco\\Documents\\Università\\Magistrale\\Intelligent Systems\\progetto\\cancer_dataset\\data.csv",
    header = T,
    sep = ","
  )

# Feature + dependent variable (diagnosis)
df.data <- df[, -1]
# Only features (indipendent variables)
df.features <- df[, c(3:32)]


#-----------------------#
#    Cleaning data      #
#-----------------------#

removeOutliers <- function (data, feature) {
  quartiles <- quantile(feature, probs = c(.25, .75), na.rm = FALSE)
  IQR <- IQR(feature)
  
  Lower <- quartiles[1] - 1.5 * IQR
  Upper <- quartiles[2] + 1.5 * IQR
  
  data_no_outlier <- subset(data, feature > Lower & feature < Upper)
  return(data_no_outlier)
}

boxplotF("Outliers", df.features)

#for (i in c("area_worst", "area_mean", "area_se", "perimeter_worst")) {
#    df.features = removeOutliers(df.features, df.features[[i]])
#}

#boxplotF("No Outliers", df.features)


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

#-----------------------#
#   Feature selection   #
#-----------------------#

# PC weight
df.pca <- prcomp(df.features, scale = TRUE, center = TRUE)
plot(df.pca, type = "lines")

ggplot(as.data.frame(df.pca$x), aes(x=PC1, y=PC2, col=df.data$diagnosis)) + geom_point(alpha=0.5)

biplot(df.pca, cex = c(.7, .7), col = c("gray", "red"))


