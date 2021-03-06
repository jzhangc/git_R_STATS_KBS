% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{rbioplot_heatmap}
\alias{rbioplot_heatmap}
\title{rbioplot_heatmap}
\usage{
rbioplot_heatmap(
  fileName,
  df = NULL,
  export_name = "data",
  Tp = "Dunnett",
  rmCntl = FALSE,
  Title = NULL,
  fontType = "sans",
  tileLow = "skyblue",
  tileHigh = "midnightblue",
  tileLbl = TRUE,
  tileLblSize = 10,
  tileTxtColour = "white",
  tileLblPos = 0.5,
  xLabel = NULL,
  xLabelSize = 10,
  xTickLblSize = 10,
  xTickItalic = FALSE,
  xTickBold = FALSE,
  xSpace = 5,
  xAngle = 0,
  xhAlign = 0.5,
  xvAlign = 0.5,
  yLabel = NULL,
  yLabelSize = 10,
  yTickLblSize = 10,
  yTickItalic = FALSE,
  yTickBold = FALSE,
  legendSize = 9,
  legendTtl = FALSE,
  legendTtlSize = 9,
  legendPos = "bottom",
  plotWidth = 170,
  plotHeight = 150
)
}
\arguments{
\item{fileName}{Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.}

\item{df}{Input data frame. Instead of a \code{csv} file,a data frame can be used directly within R.}

\item{Tp}{Type of the intended statistical test. Be sure to type with quotation marks. Options are: "t-test", "Tukey" and "Dunnett" (Case insensitive). Default is "Dunnett".}

\item{rmCntl}{Remove the first column (i.e., control). Default is \code{FALSE}.}

\item{Title}{The displayed title on top of the plot. Be sure to type with quotation marks. Default is \code{NULL}.}

\item{fontType}{The type of font in the figure. Default is "sans". For all options please refer to R font table, which is avaiable on the website: \url{http://kenstoreylab.com/?page_id=2448}.}

\item{tileLow}{Set the colour for the lower limit of the heatmap. Default is \code{skyblue}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=2448}.}

\item{tileHigh}{Set the colour for the upper limit of the heatmap. Default is \code{midnightblue}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=2448}.}

\item{tileLbl}{Enable or disable significant notation on the tiles. Default is \code{TRUE}.}

\item{tileLblSize}{Set the font size of the tile label. Default is \code{10}.}

\item{tileTxtColour}{Set the colour of the on tile label. Default is \code{"white"}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=2448}.}

\item{tileLblPos}{Set the position of the tile lables. Options are \code{0}, \code{0.5} and \code{1}. Default is \code{0.5}.}

\item{xLabel}{x axis label. Type with quotation marks. Default is \code{NULL}.}

\item{xLabelSize}{x axis label size. Default is \code{10}.}

\item{xTickLblSize}{Font size of x axis ticks. Default is 10.}

\item{xTickItalic}{Set x axis tick font to italic. Default is \code{FALSE}.}

\item{xTickBold}{Set x axis tick font to bold. Default is \code{FALSE}.}

\item{xSpace}{Set the space between the plot and the x-axis tick marks. Default is \code{5}.}

\item{xAngle}{The rotation angle (degrees) of the x axis marks. Default is \code{0} - horizontal.}

\item{xhAlign}{The horizontal alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1}, as well as the values in-between. The default value at \code{0} is especially useful when \code{xAngle = 90}.}

\item{xvAlign}{The vertical alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1},  as well as the values in-between. The default value at \code{0} is especially useful when \code{xAngle = 90}.}

\item{yLabel}{y axis label. Type with quotation marks. Default is \code{NULL}.}

\item{yLabelSize}{y axis label size. Default is \code{10}.}

\item{yTickLblSize}{Font size of y axis ticks. Default is 10.}

\item{yTickItalic}{Set y axis tick font to italic. Default is \code{FALSE}.}

\item{yTickBold}{Set y axis tick font to bold. Default is \code{FALSE}.}

\item{legendSize}{Legend size. Default is \code{9}.}

\item{legendTtl}{Hide/Display legend title. Default is \code{FALSE}.}

\item{legendTtlSize}{Set when \code{legendTtl = TRUE}, font size of the legend title. Default is \code{9}.}

\item{legendPos}{Set the legend position. Options are \code{"top"}, \code{"bottom"}, \code{"left"} and \code{"right"}. Default is \code{"bottom"}.}

\item{plotWidth}{The width of the plot (unit: mm). Default is 170. Default will fit most of the cases.}

\item{plotHeight}{The height of the plot (unit: mm). Default is 150. Default will fit most of the cases.}

\item{df_export_name}{Effective only when \code{df} is set, prefix for the export file name.}

\item{y_custom_tick_range}{To initiate setting the custom \code{y_upper_limit}, \code{y_lower_limit}, \code{y_major_tick_range}, \code{y_n_minor_ticks}. Default is \code{FALSE}.}

\item{y_upper_limit}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom upper limit for y axis. Value can be obtained from \code{\link{autorange_bar_y}}.}

\item{y_lower_limit}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom lower limit for y axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_bar_y}}.}

\item{y_major_tick_range}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom major tick range for y axis.  Value can be obtained from \code{\link{autorange_bar_y}}.}

\item{y_n_minor_ticks}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_bar_y}}.}
}
\value{
Outputs a \code{.csv} file with detailed metrics for the plot, including normalized mean and significance labels, as well as a plot image file (\code{.pdf}), with 600 dpi resolution.
}
\description{
A function for plotting simple heatmap basing on the statistical analysis of choice.
}
\details{
For input data, only one of \code{fileName} and \code{df} can be set.
}
\examples{
\dontrun{
rbioplot_heatmap("data.csv", Tp = "Tukey",
yLabel = "Relative fluorescence level")

}
}
