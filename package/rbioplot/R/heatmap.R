#' @title rbioplot_heatmap
#'
#' @description A function for plotting simple heatmap basing on the statistical analysis of choice.
#' @param fileName Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Tp Type of the intended statistical test. Case sensitive and be sure to type with quotation marks. Options are: "t-test", "Tukey" and "Dunnett". Default is "Dunnett".
#' @param Title The displayed title on top of the plot. Be sure to type with quotation marks. Default is \code{NULL}.
#' @param fontType The type of font in the figure. Default is "sans". For all options please refer to R font table, which is avaiable on the website: \url{http://kenstoreylab.com/?page_id=69}.
#' @param tileLow Set the colour for the lower limit of the heatmap. Default is \code{skyblue}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=69}.
#' @param tileHigh Set the colour for the upper limit of the heatmap. Default is \code{midnightblue}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=69}.
#' @param tileLbl Enable or disable significant notation on the tiles. Default is \code{TRUE}.
#' @param tileLblSize Set the font size of the tile label. Default is \code{10}.
#' @param tileTxtColour Set the colour of the on tile label. Default is \code{"white"}. For full colour options and names, refer to the website \url{http://kenstoreylab.com/?page_id=69}.
#' @param xLabel x axis label. Type with quotation marks. Default is \code{NULL}.
#' @param xTickLblSize Font size of x axis ticks. Default is 10.
#' @param xTickItalic Set x axis tick font to italic. Default is \code{FALSE}.
#' @param xAngle The rotation angle (degrees) of the x axis marks. Default is \code{0} - horizontal.
#' @param xAlign The alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1}. The default value at \code{0} is especially useful when \code{xAngle = 90}.
#' @param yLabel y axis label. Type with quotation marks. Default is \code{NULL}.
#' @param yTickLblSize Font size of y axis ticks. Default is 10.
#' @param yTickItalic Set y axis tick font to italic. Default is \code{FALSE}.
#' @param legendTtl Hide/Display legend title. Default is \code{FALSE}.
#' @param plotWidth The width of the plot (unit: mm). Default is 170. Default will fit most of the cases.
#' @param plotHeight The height of the plot (unit: mm). Default is 150. Default will fit most of the cases.
#' @param y_custom_tick_range To initiate setting the custom \code{y_upper_limit}, \code{y_lower_limit}, \code{y_major_tick_range}, \code{y_n_minor_ticks}. Default is \code{FALSE}.
#' @param y_upper_limit Can only be set when \code{y_custom_tick_range = TRUE}. Set custom upper limt for y axis. Value can be obtained from \code{\link{autorange_bar_y}}.
#' @param y_lower_limit Can only be set when \code{y_custom_tick_range = TRUE}. Set custom lower limt for y axis. Default is \code{0}. Value can be obtained from \code{\link{autorange_bar_y}}.
#' @param y_major_tick_range Can only be set when \code{y_custom_tick_range = TRUE}. Set custom major tick range for y axis.  Value can be obtained from \code{\link{autorange_bar_y}}.
#' @param y_n_minor_ticks Can only be set when \code{y_custom_tick_range = TRUE}. Set custom numbers of minor ticks. Default is \code{4}. Value can be obtained from \code{\link{autorange_bar_y}}.
#' @return Outputs a \code{.csv} file with detailed metrics for the plot, including Mean, SEM and significance labels, as well as a plot image file (\code{.pdf}), with 600 dpi resolution.
#' @importFrom reshape2 melt
#' @importFrom multcompView multcompLetters
#' @importFrom multcomp glht mcp
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @import ggplot2
#' @examples
#' \dontrun{
#' rbioplot("data.csv", Tp = "Tukey",
#' yLabel = "Relative fluorescence level")
#'
#' rbioplot("data2.csv", Tp = "t-test", xAngle = -90,
#' xAlign=0,yLabel="Relative fluorescence level")
#'
#' rbioplot("data3.csv", Tp = "Tukey",
#' yLabel = "Relative fluorescence level")
#'
#' rbioplot("data4.csv", Tp = "Dunnett",
#' yLabel = "Relative fluorescence level")
#'
#' rbioplot("data5.csv", Tp = "Tukey",
#' yLabel = "Relative fluorescence level", plotWidth = 300)
#'
#' rbioplot("data8.csv", Tp = "Tukey", errorbar = "SD"
#' yLabel = "Relative fluorescence level",
#' y_custom_tick_range = TRUE, y_upper_limit = 4,
#' y_lower_limit = 0, y_major_tick_range = 1,
#' y_n_minor_ticks = 4)
#' }
#' @export
rbioplot_heatmap <- function(fileName, Tp = "Dunnett",
                     Title = NULL,  fontType = "sans",
                     tileLow = "skyblue", tileHigh = "midnightblue", tileLbl = TRUE, tileLblSize = 10, tileTxtColour = "white",
                     xLabel = NULL, xTickLblSize = 10, xTickItalic = FALSE, xAngle = 0, xAlign = 0.5,
                     yLabel = NULL, yTickLblSize = 10, yTickItalic = FALSE,
                     legendTtl = FALSE,
                     plotWidth = 170, plotHeight = 150){

  ## load file
  rawData <- read.csv(file = fileName,header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]] <- factor(rawData[[1]],levels = c(unique(rawData[[1]])))

  ## normalize everything to control as 1
  Mean <- sapply(colnames(rawData)[-1],
                 function(i) tapply(rawData[[i]], rawData[1], mean, na.rm = TRUE))
  Mean <- data.frame(Mean)
  Mean$Condition <- factor(rownames(Mean), levels = c(rownames(Mean)))
  MeanNrm <- data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                               function(i)sapply(Mean[[i]], function(j)j/Mean[[i]][1])),
                        Condition = factor(rownames(Mean), levels = c(rownames(Mean))))

  ## for automatic significant labels (Tukey: letters; t-test & Dunnett: asterisks)
  cNm <- colnames(rawData)

  Tt <- sapply(colnames(rawData)[-1],
               function(i) {
                 fml<-paste(i,cNm[1], sep = "~")
                 Mdl<-aov(formula(fml), data = rawData)

                 if (Tp == "t-test"){
                   if (nlevels(rawData[[1]]) == 2){
                     Control <- subset(rawData[i], rawData[[1]] == levels(rawData[[1]])[1])
                     Experimental <- subset(rawData[i], rawData[[1]] == levels(rawData[[1]])[2])
                     Ttest <- t.test(Control, Experimental, var.equal = TRUE, na.rm = TRUE)
                     Ttestp <- Ttest$p.value
                     Lvl <- data.frame(Condition = unique(rawData[[1]]), pvalue = c(1, Ttestp))
                     Lvl$Lbl <- sapply(Lvl$pvalue, function(x)ifelse(x < 0.05, "*", ""))
                     Lvl <- Lvl[,c(1,3)]
                   } else {stop("T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try Tukey or Dunnett).")}
                 } else if (Tp == "Tukey"){
                   if (nlevels(rawData[[1]]) > 2){
                     Sts <- TukeyHSD(Mdl)
                     Tkp <- Sts[[1]][,4]
                     names(Tkp) <- sapply(names(Tkp), function(j)revsort(j))
                     Tkp <- multcompLetters(Tkp)["Letters"]
                     Lbl <- names(Tkp[["Letters"]])
                     Lvl <- data.frame(Lbl, Tkp[["Letters"]],
                                       stringsAsFactors = FALSE)
                   } else {stop("USE T-TEST FOR A TWO-GROUP COMPARISON")}
                 } else if (Tp == "Dunnett"){
                   if (nlevels(rawData[[1]]) > 2){
                     var <- cNm[1]
                     arg <- list("Dunnett")
                     names(arg) <- var
                     mcp <- do.call(mcp, arg)
                     Sts <- summary(glht(Mdl, linfct = mcp))
                     Dnt <- Sts$test$pvalues
                     names(Dnt) <- names(Sts$test$coefficients)
                     Lvl <- data.frame(Condition = unique(rawData[[1]]),pvalue = c(1,Dnt))
                     Lvl$Lbl <- sapply(Lvl$pvalue, function(x)ifelse(x < 0.05, "*", ""))
                     Lvl <- Lvl[, c(1, 3)]
                   } else {stop("USE T-TEST FOR A TWO-GROUP COMPARISON")}
                 } else {
                   stop("ERROR: CHECK YOUR SPELLING (Hint: EveRyThinG iS cASe-sEnSiTiVE).")
                 }
                 colnames(Lvl) <- c(colnames(rawData)[1], i)
                 Lvl
               },simplify = FALSE)
  cTt <- Reduce(function(x, y) merge(x, y, all = TRUE,
                                     by = colnames(rawData)[1],sort = FALSE),
                Tt, accumulate = FALSE)
  colnames(cTt)[-1] <- sapply(colnames(rawData)[-1],
                              function(x)paste(x, "Lbl", sep=""))

  ## generate the master dataframe for plotting
  MeanNrmMLT <- melt(MeanNrm,id.vars = colnames(MeanNrm)[length(colnames(MeanNrm))])
  MeanNrmMLT$id <- rownames(MeanNrmMLT)

  cTtMLT <- melt(cTt,id.vars = colnames(cTt)[1])
  cTtMLT$id <- rownames(cTtMLT)
  cTtMLT[1] <- as.factor(cTtMLT[[1]])

  colnames(MeanNrmMLT)[3] <- "NrmMean"
  colnames(cTtMLT)[1:3] <- c(colnames(MeanNrmMLT)[1], "variableLbl", "Lbl")

  DfPlt <- merge(MeanNrmMLT, cTtMLT, by = c("id", "Condition"), sort = FALSE)

  # dump all data into a file
  write.csv(DfPlt,file = paste(substr(noquote(fileName), 1, nchar(fileName) - 4), ".plot.heatmap.csv",sep= ""),
            quote = FALSE, na = "NA", row.names = FALSE)

  ## plotting
  loclEnv <- environment()

  baseplt <- ggplot(data = DfPlt, aes(x = Condition, y = variable),
                    environment = loclEnv) +
    geom_tile(aes(fill = NrmMean), colour = "white") +
    scale_fill_gradient(low = tileLow, high = tileHigh,
                        breaks = c(ceiling(with(DfPlt, min(NrmMean) / 0.05)) * 0.05, floor(with(DfPlt, max(NrmMean)) / 0.05) * 0.05),
                        guide = guide_colorbar(ticks = FALSE)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    ggtitle(Title) +
    xlab(xLabel) +
    ylab(yLabel) +
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.border = element_blank(),
          axis.ticks = element_line(colour = "white"),
          plot.title = element_text(face = "bold", family = fontType),
          axis.title = element_text(face = "bold", family = fontType),
          legend.position = "bottom",
          axis.text.x = element_text(size = xTickLblSize, family = fontType, angle = xAngle, hjust = xAlign,
                                     margin = margin(t = 5, r = 5, b = 3, l = 5, unit = "pt")),
          axis.text.y = element_text(size = yTickLblSize, family = fontType, hjust = 0.5))

  if (tileLbl == TRUE){
    baseplt <- baseplt +
      geom_text(aes(x = Condition, y = variable,label = Lbl), size = tileLblSize, color = tileTxtColour, family = fontType)
  }

  if (xTickItalic == TRUE){
    baseplt <- baseplt +
      theme(axis.text.x = element_text(face = "italic"))
  }

  if (yTickItalic == TRUE){
    baseplt <- baseplt +
      theme(axis.text.y = element_text(face = "italic"))
  }

  if (legendTtl == FALSE){
    pltLbl<-baseplt + theme(legend.title = element_blank())
  } else {
    pltLbl<-baseplt + theme(legend.title = element_text(size = 9))
  }

  if (nlevels(DfPlt$variable) == 1){
    plt<-pltLbl +
      theme(axis.text.x = element_blank()) +
      coord_equal(ratio = 0.5) +
      scale_x_discrete(expand = c(0.1, 0.1)) # space between y axis and fist/last bar
  } else {
    plt<-pltLbl
  }

  ## switch x axis to top
  grid.newpage()

  # extract gtable
  pltgtb <- ggplot_gtable(ggplot_build(plt))

  # switch x axis to top
  Ap <- c(subset(pltgtb$layout, name == "panel", se = t:r))
  Aa <- which(pltgtb$layout$name == "axis-b")
  pltgtb_a <- pltgtb$grobs[[Aa]]
  axs <- pltgtb_a$children[[2]]
  axs$heights <- rev(axs$heights)
  axs$grobs <- rev(axs$grobs)
  axs$grobs[[2]]$y <- axs$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
  pltgtb <- gtable_add_rows(pltgtb, pltgtb$heights[pltgtb$layout[Aa, ]$l], Ap$t - 1)
  pltgtb <- gtable_add_grob(pltgtb, axs, l = Ap$l, t = Ap$t, r = Ap$r)
  pltgtb <- pltgtb[-(Ap$b + 2), ]

  # export the file and draw a preview
  ggsave(filename = paste(substr(noquote(fileName), 1, nchar(fileName) - 4),".plot.heatmap.pdf", sep=""), plot = pltgtb,
         width = plotWidth, height = plotHeight, units = "mm",dpi = 600)
  grid.draw(pltgtb)
}
