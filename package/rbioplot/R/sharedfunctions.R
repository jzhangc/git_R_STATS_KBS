#' @title revsort
#'
#' @description A function swtiches the order around the "-" symbol of a character string: from "a-b" to "b-a".
#' @param x character string with \code{"-"}
#' @return Outputs a \code{string} with reversed order around \code{"-"}. Note that the function only apply to the first \code{"-"}. In the case of mulitple \code{"-"}, it will cut the rest off.
#' @examples
#' \dontrun{
#' ab<-"a-b"
#' ab<-revsort(ab)
#' ab
#' }
#' @export
revsort <- function(x){
  uLst <- unlist(strsplit(x, "-"))
  uLst <- uLst[c(2,1)]
  uLst <- paste(uLst,collapse = "-")
  uLst
}

#' @title minor_tick
#'
#' @description A function to calculate space for minor ticks
#' @param major A vector of major ticks
#' @param n_minor Number of minor ticks
#' @return A vector containing the spaces for minor ticks
#' @examples
#' \dontrun{
#' minor_tick(seq(1, 10, by = 2), 4)
#' }
#' @export
minor_tick <- function(major, n_minor){
  labs <- c(sapply(major, function(x) c(x, rep("", n_minor))))
  labs[1:(length(labs) - n_minor)]
}


#' @title rightside_y
#'
#' @description A function to calculate space for minor ticks
#' @param ggobject Input ggplot2 object.
#' @return A ggplot2 object with rightside y axis
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @examples
#' \dontrun{
#' plt <- rightside_y(plt)
#' }
#' @export
rightside_y <- function(ggobject){
  # check object type
  if (!any(class(ggobject) %in% c("gg", "gglot"))){
    stop("this rightside y-axis function only works on ggplot2 objects.")
  }

  # extract gtable
  pltgtb <- ggplot_gtable(ggplot_build(ggobject))
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

  # output
  return(pltgtb)
}


#' @title multi_plot_shared_legend
#'
#' @description A function to calculate space for minor ticks
#' @param ggobject_list Input ggplot2 object list containing multiple ggplot objectes.
#' @param ncol Number of columns in one figure page.
#' @param nrow Number of rows in one figure page.
#' @param position Legend position. Options are \code{"bottom"} or \code{"right"}.
#' @return A grob object with mulitple ggplot2 plots in one page with shared legends.
#' @details This is a modified version from tidyverse: \url{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-grap}
#' @import gridExtra
#' @import ggplot2
#' @importFrom grid unit.c
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @examples
#' \dontrun{
#' combined_plot <- multi_plot_shared_legend(pltlist)
#' }
#' @export
multi_plot_shared_legend <- function(ggobject_list, ncol = length(ggobject_list), nrow = 1, position = c("bottom", "right")) {
  plots <- ggobject_list
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  # return gtable invisibly
  return(combined)
}
