eval_metrics <- function(model, df, predictions, target)
{
  resids <- df[,target] - predictions
  resids2 <- resids**2
  N <- length(predictions)
  r2 <- as.character(round(summary(model)$r.squared, 2))
  adj_r2 <- as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}