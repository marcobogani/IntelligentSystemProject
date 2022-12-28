
#' Formatted BoxPlot
#'
#' @param title Title of boxplot
#' @param data Data to be plotted
#'
#' @return
#' @export
#'
#' @examples
boxplotF <- function (title, data) {
  ## Adjust some graphical parameters.
  par(mar = c(4.1, 4.1, 4.1, 4.1), # change the margins
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
       # TODO: Align xlabel
       adj = 1.1,
       ## Increase label size.
       cex = 1)
}
