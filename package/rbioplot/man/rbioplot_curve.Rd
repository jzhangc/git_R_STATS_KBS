% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frogcurves.R
\name{rbioplot_curve}
\alias{rbioplot_curve}
\title{rbioplot_curve}
\usage{
rbioplot_curve(
  fileName,
  df = NULL,
  export_name = "data",
  Title = NULL,
  errorbar = "SEM",
  errorbarWidth = 0.2,
  fontType = "sans",
  symbolSize = 2,
  xLabel = NULL,
  xLabelSize = 10,
  xTickLblSize = 10,
  xTickItalic = FALSE,
  xTickBold = FALSE,
  xAngle = 0,
  xhAlign = 0.5,
  xvAlign = 0.5,
  rightsideY = TRUE,
  yLabel = NULL,
  yLabelSize = 10,
  yTickLblSize = 10,
  yTickItalic = FALSE,
  yTickBold = FALSE,
  legendSize = 9,
  legendTtl = FALSE,
  legendTtlSize = 9,
  plotWidth = 170,
  plotHeight = 150,
  x_custom_tick_range = FALSE,
  x_lower_limit = 0,
  x_upper_limit,
  x_major_tick_range,
  x_n_minor_ticks = 0,
  y_custom_tick_range = FALSE,
  y_lower_limit = 0,
  y_upper_limit,
  y_major_tick_range,
  y_n_minor_ticks = 4
)
}
\arguments{
\item{fileName}{Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files. Note that the column names (excluding the first column) need to be numeric.}

\item{df}{Input data frame. Instead of a \code{csv} file,a data frame can be used directly within R.}

\item{Title}{The displayed title on top of the plot. Be sure to type with quotation marks. Default is \code{NULL}.}

\item{errorbar}{Set the type of errorbar. Options are standard error of the mean (\code{"SEM"}, \code{"standard error"}, \code{"standard error of the mean"}), or standard deviation (\code{"SD"}, \code{"standard deviation"}), case insensitive. Default is \code{"SEM"}.}

\item{errorbarWidth}{Set the width for errorbar. Default is \code{0.2}.}

\item{fontType}{The type of font in the figure. Default is "sans". For all options please refer to R font table, which is available on the website: \url{http://kenstoreylab.com/?page_id=2448}.}

\item{symbolSize}{Set the size of symbols. Default is \code{2}.}

\item{xLabel}{x axis label. Type with quotation marks. Default is \code{NULL}.}

\item{xLabelSize}{x axis label size. Default is \code{10}.}

\item{xTickLblSize}{Font size of x axis ticks. Default is \code{10}.}

\item{xTickItalic}{Set x axis tick font to italic. Default is \code{FALSE}.}

\item{xTickBold}{Set x axis tick font to bold. Default is \code{FALSE}.}

\item{xAngle}{The rotation angle (degrees) of the x axis marks. Default is \code{0} - horizontal.}

\item{xhAlign}{The horizontal alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1}, as well as the values in-between. The default value at \code{0} is especially useful when \code{xAngle = 90}.}

\item{xvAlign}{The vertical alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1},  as well as the values in-between. The default value at \code{0} is especially useful when \code{xAngle = 90}.}

\item{rightsideY}{If to display the right side y-axis. Default is \code{TRUE}.}

\item{yLabel}{y axis label. Type with quotation marks. Default is \code{NULL}.}

\item{yLabelSize}{y axis label size. Default is \code{10}}

\item{yTickLblSize}{Font size of y axis ticks. Default is 10.}

\item{yTickItalic}{Set y axis tick font to italic. Default is \code{FALSE}.}

\item{yTickBold}{Set y axis tick font to bold. Default is \code{FALSE}.}

\item{legendSize}{Legend size. Default is \code{9}.}

\item{legendTtl}{Hide/Display legend title. If \code{TRUE} or \code{T}, the name of the first column of the raw data file will display as the legend title. Default is \code{FALSE}.}

\item{legendTtlSize}{Set when \code{legendTtl = TRUE}, font size of the legend title. Default is \code{9}.}

\item{plotWidth}{The width of the plot (unit: mm). Default is 170. Default will fit most of the cases.}

\item{plotHeight}{The height of the plot (unit: mm). Default is 150. Default will fit most of the cases.}

\item{x_custom_tick_range}{To initiate setting the custom \code{x_upper_limit}, \code{x_lower_limit}, \code{x_major_tick_range}, \code{x_n_minor_ticks}. Default is \code{FALSE}.}

\item{x_lower_limit}{Can only be set when \code{x_custom_tick_range = TRUE}. Set custom lower limit for x axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_curve}}.}

\item{x_upper_limit}{Can only be set when \code{x_custom_tick_range = TRUE}. Set custom upper limit for x axis. Value can be obtained from \code{\link{autorange_curve}}.}

\item{x_major_tick_range}{Can only be set when \code{x_custom_tick_range = TRUE}. Set custom major tick range for x axis.  Value can be obtained from \code{\link{autorange_curve}}.}

\item{x_n_minor_ticks}{Can only be set when \code{x_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_curve}}.}

\item{y_custom_tick_range}{To initiate setting the custom \code{y_upper_limit}, \code{y_lower_limit}, \code{y_major_tick_range}, \code{y_n_minor_ticks}. Default is \code{FALSE}.}

\item{y_lower_limit}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom lower limit for y axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_curve}}.}

\item{y_upper_limit}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom upper limit for y axis. Value can be obtained from \code{\link{autorange_curve}}.}

\item{y_major_tick_range}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom major tick range for y axis.  Value can be obtained from \code{\link{autorange_curve}}.}

\item{y_n_minor_ticks}{Can only be set when \code{y_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_curve}}.}

\item{df_export_name}{Effective only when \code{df} is set, prefix for the export file name.}
}
\value{
Outputs a \code{.csv} file with detailed metrics for the plot, including Mean and SEM, as well as a plot image file (\code{.pdf}), with 600 dpi resolution.
}
\description{
A simple to use function for plotting joining-point curve figures with continuous x and y axises values.
}
\details{
For input data, only one of \code{fileName} and \code{df} can be set.
}
\examples{
\dontrun{
rbioplot_curve("data6.csv", legendTtl = TRUE,
          y_custom_tick_range = TRUE, y_upper_limit = 45,
          y_major_tick_range = 5,
          y_n_minor_ticks = 4,
          x_custom_tick_range = TRUE, x_upper_limit = 35,
          x_major_tick_range = 5)
}
}
