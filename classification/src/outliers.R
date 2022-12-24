#' Remove Outliers
#'
#' @param data This is the dataset
#' @param feature This is the feature name to inspect
#'
#' @return the dataset without outliers of the specified feature
#' @export
#'
#' @examples
#' remove_outliers(df, "area_mean")
remove_outliers <- function (data, feature_name) {
  feature <- data[[feature_name]]
  quartiles <- quantile(feature, probs = c(.25, .75), na.rm = FALSE)
  IQR <- IQR(feature)
  
  # Find Lower Limit for outliers
  Lower <- quartiles[1] - 1.5 * IQR
  # Find Upper Limit for outliers
  Upper <- quartiles[2] + 1.5 * IQR
  # Remove outliers
  data_no_outlier <- subset(data, feature > Lower & feature < Upper)
  return(data_no_outlier)
}
