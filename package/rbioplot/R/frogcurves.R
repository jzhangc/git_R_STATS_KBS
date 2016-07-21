#' @title autorange_curve
#'
#' @description A function to get custom lower/upper limit, major tick range, as well as minor tick options for both axises of a joint-piont curve with continuous x AND y values, based on a user-defined major tick number.
#' @param fileName Input file name. Data should be arranged same as the input file for \code{\link{rbioplot_curve}}.Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files. Note that the column names (excluding the first column) need to be numeric.
#' @param x_nMajorTicks Number of major ticks intended to use for the x axis. Note that the input number should be major tick number EXCLUDING 0 (or x axis lower limit if not using 0). Default is \code{5}. Note: Depending on the raw range, the last label may or may not show up due to plotting optimization, see \code{\link{rbioplot_curve}}.
#' @param x_DfltZero When \code{TRUE}, start x axis from \code{0}. Default is \code{TRUE}.
#' @param y_nMajorTicks Number of major ticks intended to use for the y axis. Note that the input number should be major tick number EXCLUDING 0 (or y axis lower limit if not using 0). Default is \code{10}. Note: Depending on the raw range, the last label may or may not show up due to plotting optimization, see \code{\link{rbioplot_curve}}.
#' @param y_DfltZero When \code{TRUE}, start y axis from \code{0}. Default is \code{TRUE}.
#' @importFrom reshape2 melt
#' @return A list object containing \code{lower_limit}, \code{upper_limit}, \code{major_tick_range} and \code{minor_tick_options} for both axises.
#' @examples
#' \dontrun{
#' enzyme_curve("data6.csv", x_nMajorTicks = 6, x_DfltZero = FALSE,
#' y_nMajorTicks = 8, y_DfltZero = TRUE)
#' }
#' @export
autorange_curve<-function(fileName, x_nMajorTicks = 5, x_DfltZero = TRUE,
                      y_nMajorTicks = 10, y_DfltZero = TRUE){

  ## load file
  rawData<-read.csv(file = fileName, header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]], levels = c(unique(rawData[[1]])))

  ## calculate mean and SEM
  Mean<-sapply(colnames(rawData)[-1],
                 function(i) tapply(rawData[[i]], rawData[1], mean, na.rm=TRUE))
  Mean<-data.frame(Mean)
  Mean$Condition<-factor(rownames(Mean), levels = c(rownames(Mean)))


  SEM<-sapply(colnames(rawData)[-1],
                function(i) tapply(rawData[[i]], rawData[1],
                                   function(j)sd(j,na.rm = TRUE) / sqrt(length(!is.na(j)))))
  SEM<-data.frame(SEM)
  SEM$Condition<-factor(rownames(SEM),levels = c(rownames(SEM)))

  ## generate the master dataframe
  MeanMLT<-melt(Mean,id.vars = colnames(Mean)[length(colnames(Mean))])
  MeanMLT$id<-rownames(MeanMLT)

  SEMMLT<-melt(SEM,id.vars = colnames(SEM)[length(colnames(SEM))])
  SEMMLT$id<-rownames(SEMMLT)

  colnames(MeanMLT)[3]<- "plotMean"
  colnames(SEMMLT)[2:3]<-c("variableSEM","plotSEM")


  DfPlt<-merge(MeanMLT,SEMMLT,by = c("id","Condition"),sort=FALSE)

  DfPlt$variable<-as.character(DfPlt$variable)
  DfPlt$variable<-sapply(DfPlt$variable,function(x)substr(x,2,nchar(x)))
  DfPlt$variable<-as.numeric(DfPlt$variable)

  ## calculate optimal lower/upper limits (x_lw_lmt/x_upr_lmt) and major tick range (x_rd_intvl) for x axis
  # setting the raw x lower/upper limit
  ifelse(x_DfltZero == FALSE, x_Mn<-with(DfPlt, floor(min(unique(variable)) / 0.5) * 0.5), x_Mn<-0)
  x_Mx<-with(DfPlt, ceiling((max(unique(variable)) + 0.02) / 0.5) * 0.5)

  x_Rge<-x_Mx - x_Mn
  x_raw_intvl<-x_Rge / x_nMajorTicks # nMjoarTicks: excluding 0 (or the ymin)
  x_Aa<-10^ceiling(log10(x_raw_intvl))
  x_rd_intvl<-ceiling((x_raw_intvl / x_Aa) / 0.05) * 0.05 * x_Aa
  x_lw_lmt<-x_rd_intvl * floor(x_Mn / x_rd_intvl)
  x_upr_lmt<-x_rd_intvl * ceiling(x_Mx / x_rd_intvl)

  ## calculate minor tick options for x axis
  # set 4 as the minor tick range cutoff: it always makes sure to give at least 4 minor ticks.
  # if not, a decimal sacling factor (i=1) will be applied
  if (max(sapply(all_dvsr(x_rd_intvl), function(i)x_rd_intvl / i-1)) < 4){
    x_minor_tick_n<-sapply(all_dvsr(x_rd_intvl, 1), function(i)round(x_rd_intvl/i - 1))
  } else {
    x_minor_tick_n<-sapply(all_dvsr(x_rd_intvl), function(i)round(x_rd_intvl/i - 1))
  }


  ## calculate optimal lower/upper limits (y_lw_lmt/y_upr_lmt) and major tick range (y_rd_intvl) for y axis
  # setting the raw y lower/upper limit
  ifelse(y_DfltZero == FALSE, y_Mn<-with(DfPlt, floor(min(plotMean - ifelse(is.na(plotSEM), 0, plotSEM)) / 0.5) * 0.5), y_Mn<-0)
  y_Mx<-ceiling(with(DfPlt, max(plotMean + ifelse(is.na(plotSEM), 0, plotSEM)) + 0.02) / 0.5) * 0.5

  y_Rge<-y_Mx - y_Mn
  y_raw_intvl<-y_Rge / y_nMajorTicks # nMjoarTicks: excluding 0 (or the ymin)
  y_Aa<-10^ceiling(log10(y_raw_intvl))
  y_rd_intvl<-ceiling((y_raw_intvl / y_Aa) / 0.05) * 0.05 * y_Aa
  y_lw_lmt<-y_rd_intvl * floor(y_Mn / y_rd_intvl)
  y_upr_lmt<-y_rd_intvl * ceiling(y_Mx / y_rd_intvl)

  ## calculate minor tick options for y axis
  # set 4 as the minor tick range cutoff: it always makes sure to give at least 4 minor ticks.
  # if not, a decimal sacling factor (i=1) will be applied
  if (max(sapply(all_dvsr(y_rd_intvl), function(i)y_rd_intvl / i-1)) < 4){
    y_minor_tick_n<-sapply(all_dvsr(y_rd_intvl, 1), function(i)round(y_rd_intvl/i - 1))
  } else {
    y_minor_tick_n<-sapply(all_dvsr(y_rd_intvl), function(i)round(y_rd_intvl/i - 1))
  }

  ## results
  Lst<-list(x_axis_range = c(x_lw_lmt, x_upr_lmt, x_rd_intvl), x_minor_tick_options = sort(x_minor_tick_n),
            y_axis_range = c(y_lw_lmt, y_upr_lmt, y_rd_intvl), y_minor_tick_options = sort(y_minor_tick_n))
  names(Lst[[1]])<-c("x_lower_limit", "x_upper_limit", "x_major_tick_range")
  names(Lst[[3]])<-c("y_lower_limit", "y_upper_limit", "y_major_tick_range")

  return(Lst)
}


#' @title rbioplot_curve
#'
#' @description A simple to use function for plotting joining-point curve figures with continuous x and y axises values.
#' @param fileName Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files. Note that the column names (excluding the first column) need to be numeric.
#' @param Title The displayed title on top of the plot. Be sure to type with quotation marks. Default is \code{NULL}.
#' @param xLabel x axis label. Type with quotation marks. Default is \code{NULL}.
#' @param xTickLblSize Font size of x axis ticks. Default is 10.
#' @param xAngle The rotation angle (degrees) of the x axis marks. Default is \code{0} - horizontal.
#' @param xAlign The alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1}. The default value at \code{0} is especially useful when \code{xAngle = 90}.
#' @param yLabel y axis label. Type with quotation marks. Default is \code{NULL}.
#' @param yTickLblSize Font size of y axis ticks. Default is 10.
#' @param legendTtl Hide/Display legend title. If \code{TRUE} or \code{T}, the name of the first column of the raw date file will display as the legend title. Default is \code{FALSE}.
#' @param plotWidth The width of the plot (unit: mm). Default is 170. Default will fit most of the cases.
#' @param plotHeight The height of the plot (unit: mm). Default is 150. Default will fit most of the cases.
#' @param x_custom_tick_range To initiate setting the custom \code{x_upper_limit}, \code{x_lower_limit}, \code{x_major_tick_range}, \code{x_n_minor_ticks}. Default is \code{FALSE}.
#' @param x_upper_limit Can only be set when \code{x_custom_tick_range = TRUE}. Set custom upper limt for x axis. Value can be obtained from \code{\link{autorange_curve}}.
#' @param x_lower_limit Can only be set when \code{x_custom_tick_range = TRUE}. Set custom lower limt for x axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_curve}}.
#' @param x_major_tick_range Can only be set when \code{x_custom_tick_range = TRUE}. Set custom major tick range for x axis.  Value can be obtained from \code{\link{autorange_curve}}.
#' @param x_n_minor_ticks Can only be set when \code{x_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_curve}}.
#' @param y_custom_tick_range To initiate setting the custom \code{y_upper_limit}, \code{y_lower_limit}, \code{y_major_tick_range}, \code{y_n_minor_ticks}. Default is \code{FALSE}.
#' @param y_upper_limit Can only be set when \code{y_custom_tick_range = TRUE}. Set custom upper limt for y axis. Value can be obtained from \code{\link{autorange_curve}}.
#' @param y_lower_limit Can only be set when \code{y_custom_tick_range = TRUE}. Set custom lower limt for y axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_curve}}.
#' @param y_major_tick_range Can only be set when \code{y_custom_tick_range = TRUE}. Set custom major tick range for y axis.  Value can be obtained from \code{\link{autorange_curve}}.
#' @param y_n_minor_ticks Can only be set when \code{y_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_curve}}.
#' @return Outputs a \code{.csv} file with detailed metrics for the plot, including Mean and SEM, as well as a plot image file (\code{.pdf}), with 600 dpi resolution.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @examples
#' \dontrun{
#' rbioplot_curve("data6.csv", legendTtl = TRUE,
#'           y_custom_tick_range = TRUE, y_upper_limit = 45,
#'           y_major_tick_range = 5,
#'           y_n_minor_ticks = 4,
#'           x_custom_tick_range = TRUE, x_upper_limit = 35,
#'           x_major_tick_range = 5)
#' }
#' @export
rbioplot_curve<-function(fileName, Title = NULL,
                         xLabel = NULL, xTickLblSize = 10, xAngle = 0, xAlign = 0.5,
                         yLabel = NULL, yTickLblSize = 10,
                         legendTtl=FALSE, plotWidth = 170, plotHeight = 150,
                         x_custom_tick_range = FALSE, x_lower_limit = 0, x_upper_limit, x_major_tick_range, x_n_minor_ticks = 0,
                         y_custom_tick_range = FALSE, y_lower_limit = 0, y_upper_limit, y_major_tick_range, y_n_minor_ticks = 4){
  ## load file
  rawData<-read.csv(file=fileName,header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]],levels=c(unique(rawData[[1]])))

  cNm<-colnames(rawData) # load all the column names

  ## calculate mean and SEM
  Mean<-sapply(colnames(rawData)[-1],
               function(i) tapply(rawData[[i]], rawData[1], mean, na.rm=TRUE))
  Mean<-data.frame(Mean)
  Mean$Condition<-factor(rownames(Mean),levels=c(rownames(Mean)))

  SEM<-sapply(colnames(rawData)[-1],
              function(i) tapply(rawData[[i]], rawData[1],
                                 function(j)sd(j,na.rm=TRUE)/sqrt(length(!is.na(j)))))
  SEM<-data.frame(SEM)
  SEM$Condition<-factor(rownames(SEM),levels=c(rownames(SEM)))
  colnames(SEM)[-length(colnames(SEM))]<-sapply(colnames(rawData)[-1],
                                                function(x)paste(x,"SEM",sep=""))

  ## generate the master dataframe for plotting
  MeanMLT<-melt(Mean, id.vars = colnames(Mean)[length(colnames(Mean))])
  MeanMLT$id<-rownames(MeanMLT)

  SEMMLT<-melt(SEM, id.vars = colnames(SEM)[length(colnames(SEM))])
  SEMMLT$id<-rownames(SEMMLT)

  colnames(MeanMLT)[3]<- "plotMean"
  colnames(SEMMLT)[2:3]<-c("variableSEM","plotSEM")

  DfPlt<-merge(MeanMLT,SEMMLT,by = c("id","Condition"),sort=FALSE)

  DfPlt$variable<-as.character(DfPlt$variable)
  DfPlt$variable<-sapply(DfPlt$variable,function(x)substr(x,2,nchar(x)))
  DfPlt$variable<-as.numeric(DfPlt$variable)

  # dump all data into a file
  write.csv(DfPlt,file = paste(substr(noquote(fileName),1,nchar(fileName) - 4),".plot.csv",sep = ""),
            quote = FALSE,na = "NA",row.names = FALSE)

  ## plotting
  # a function to add minor ticks
  minor_tick <- function(major, n_minor) {
    labs <- c(sapply(major, function(x) c(x, rep("", n_minor))))
    labs[1:(length(labs) - n_minor)]
  }

  # x axis
  if (x_custom_tick_range == TRUE){ # custome x range and tick settings
    x_axis_Mx<-x_upper_limit
    x_axis_Mn<-x_lower_limit
    x_mj_range<-x_major_tick_range # determined from the optrange_enzyme() function - major_tick_range
    x_n_mnr<-x_n_minor_ticks # chosen from the optrange_enzyme() function - minor_tick_options
  } else {
    x_axis_Mx<-with(DfPlt, ceiling((max(unique(variable)) + 0.02) / 0.5) * 0.5)# the default x axis upper limit=max(mean+extra)
    x_axis_Mn<-0
    x_mj_range<-0.5 # default
    x_n_mnr<-0 # default
  }

  # y axis
  if (y_custom_tick_range == TRUE){ # custome y range and tick settings
    y_axis_Mx<-y_upper_limit
    y_axis_Mn<-y_lower_limit
    y_mj_range<-y_major_tick_range # determined from the optrange_enzyme() function - major_tick_range
    y_n_mnr<-y_n_minor_ticks # chosen from the optrange_enzyme() function - minor_tick_options
  } else {
    y_axis_Mx<-with(DfPlt,ceiling((max(plotMean + ifelse(is.na(plotSEM), 0, plotSEM)) + 0.02) / 0.5) * 0.5) # the default y axis upper limit=max(mean+SEM+label+extra)
    y_axis_Mn<-0
    y_mj_range<-0.5 # default
    y_n_mnr<-4 # default
  }



  loclEnv<-environment()
  baseplt<-ggplot(data=DfPlt, aes(x = variable, y = plotMean, shape = Condition, linetype = Condition),
                  environment = loclEnv)+
    geom_line()+
    geom_point()+
    geom_errorbar(aes(ymin = plotMean - ifelse(is.na(plotSEM), 0, plotSEM),
                      ymax = plotMean + ifelse(is.na(plotSEM), 0, plotSEM)), width = 0.2,
                  linetype = "solid")+
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(x_axis_Mn, x_axis_Mx, by = x_mj_range / (x_n_mnr + 1)),
                       labels = minor_tick(seq(x_axis_Mn, x_axis_Mx, by = x_mj_range), x_n_mnr),
                       limits = c(x_axis_Mn, x_axis_Mx))+
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(y_axis_Mn, y_axis_Mx, by = y_mj_range / (y_n_mnr + 1)),
                       labels = minor_tick(seq(y_axis_Mn, y_axis_Mx, by = y_mj_range), y_n_mnr),
                       limits = c(y_axis_Mn, y_axis_Mx))+
    ggtitle(Title)+
    xlab(xLabel)+
    ylab(yLabel)+
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.position = "bottom",legend.title = element_blank(),legend.key = element_blank(),
          axis.text.x = element_text(size = xTickLblSize, angle = xAngle, hjust = xAlign),
          axis.text.y = element_text(size = yTickLblSize, hjust = 0.5))+
    scale_shape_manual(name=cNm[1],values = c(5:(5 + length(unique(DfPlt$Condition)))))+
    scale_linetype_manual(name=cNm[1],values = c(1:(1 + length(unique(DfPlt$Condition)))))

  if (legendTtl == FALSE){
    plt<-baseplt + theme(legend.title = element_blank())

  } else {
    plt<-baseplt + theme(legend.title = element_text(size=9))
  }

  ## add the right-side y axis
  grid.newpage()

  # extract gtable
  pltgtb <- ggplot_gtable(ggplot_build(plt))

  # add the right side y axis
  Aa <- which(pltgtb$layout$name == "axis-l")
  pltgtb_a <- pltgtb$grobs[[Aa]]
  axs <- pltgtb_a$children[[2]]
  axs$widths <- rev(axs$widths)
  axs$grobs <- rev(axs$grobs)
  axs$grobs[[1]]$x <- axs$grobs[[1]]$x - unit(1, "npc") + unit(0.08, "cm")
  Ap <- c(subset(pltgtb$layout, name == "panel", select = t:r))
  pltgtb <- gtable_add_cols(pltgtb, pltgtb$widths[pltgtb$layout[Aa, ]$l], length(pltgtb$widths) - 1)
  pltgtb <- gtable_add_grob(pltgtb, axs, Ap$t, length(pltgtb$widths) - 1, Ap$b)

  # export the file and draw a preview
  ggsave(filename = paste(substr(noquote(fileName), 1, nchar(fileName) - 4), ".plot.pdf", sep = ""), plot = pltgtb,
         width = plotWidth, height = plotHeight, units = "mm",dpi=600)
  grid.draw(pltgtb) # preview
}
