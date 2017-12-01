#' @title set_hue
#' @description Intermediate function enables the colour change functionality for \code{\link{rbioplot_app}}.
#' @param n Number of variables to plot.
#' @examples
#' \dontrun{
#' col <- set_hue(n = 4) # launch the app version by running the function
#' }
#' @export
set_hue <- function(n) {  # colour picking function if !greyScale
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' @title rbioplot_app
#'
#' @description The web app version of \code{\link{rbioplot}}.
#' @importFrom reshape2 melt
#' @importFrom multcompView multcompLetters
#' @importFrom multcomp glht mcp
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @importFrom scales rescale_none
#' @importFrom colourpicker colourInput
#' @import ggplot2
#' @import shiny
#' @examples
#' \dontrun{
#' rbioplot_app() # launch the app version by running the function
#' }
#' @export
rbioplot_app <- function(){
  app <- shinyApp(
    ui = navbarPage(inverse = TRUE,
                    title = HTML("<a style = color:white; href = \"http://kenstoreylab.com/?page_id=2448\" target = \"_blank\">FUNCTION: rbioplot</a>"),
                    tabPanel("Raw data", sidebarLayout(sidebarPanel(
                      # adjust the size and scroll
                      tags$head(
                        tags$style(type = "text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
                        tags$style(type = "text/css", "select { max-width: 200px; }"),
                        tags$style(type = "text/css", "textarea { max-width: 185px; }"),
                        tags$style(type = "text/css", ".jslider { max-width: 200px; }"),
                        tags$style(type = "text/css", ".well { max-width: 310px; }"), # size
                        tags$style(type = "text/css", ".well { min-width: 310px; }"), # size
                        tags$style(type = "text/css", ".span4 { max-width: 310px; }"),
                        tags$style(type = "text/css", "form.well { max-height: 95vh; overflow-y: auto; }") # scroll
                      ),
                      # Input: Select a file ----
                      fileInput("file1", h2("Input CSV File"), # first quotation has the name of the input argument: input$file1. Same as below
                                multiple = TRUE,
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

                      # Horizontal line ----
                      tags$hr(),

                      ## Input block
                      h2("Input file settings"),

                      # Input: Select separator ----
                      radioButtons("sep",
                                   "Separator",
                                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","), # selected = "," term sets the default value

                      # Input: Select number of rows to display ----
                      radioButtons("disp", "Display", choices = c(Head = "head", All = "all"), selected = "head"),

                      # Horizontal line ----
                      tags$hr(),
                      actionButton("close", "Close App", icon = icon("exclamation"),
                                   onclick = "setTimeout(function(){window.close();}, 100);")
                    ),
                    mainPanel(tableOutput("contents"))
                    )),

                    tabPanel("Plot", sidebarLayout(sidebarPanel(
                      ## Quick plot
                      h2("Plot"),

                      # plot: stats
                      radioButtons("Tp", "Statistical anlaysis", choices = c(`t-test` = "t-test", `ANOVA + Tukey` = "tukey", `ANOVA + Dunnett\'s` = "dunnetts"),
                                   selected = "t-test"),

                      # Buttons
                      div(style = "display:inline-block", downloadButton("dlPlot", "Save plot")),

                      # Horizontal line ----
                      tags$hr(),

                      # exit
                      actionButton("close2", "Close App", icon = icon("exclamation"),
                                   onclick = "setTimeout(function(){window.close();}, 100);"),

                      # Horizontal line ----
                      tags$hr(),

                      ## Plot settings
                      h2("Detailed plot settings"),

                      # Space ----
                      tags$br(),

                      # General
                      h4("General settings"),

                      # Plot: title
                      textInput("Title", "Plot title", value = NULL, width = NULL, placeholder = NULL),
                      numericInput(inputId = "TitleSize", label = "Plot title size", value = 10),

                      # Plot: font
                      textInput("fontType", "Font type", value = "sans", width = NULL, placeholder = NULL),
                      actionButton(inputId = "fontTable", "Font table", icon = icon("th"), onclick = "window.open('http://kenstoreylab.com/wp-content/uploads/2015/08/R-font-table.png', '_blank')"),

                      # Plot: size
                      numericInput(inputId = "plotWidth", label = "Plot width", value = 800, step = 10),
                      numericInput(inputId = "plotHeight", label = "Plot height", value = 600, step = 10),

                      # Plot: if to normalized to 1
                      checkboxInput("Nrm", "Normalize to control as 1", TRUE),

                      # Plot: right side y
                      checkboxInput("rightsideY", "Display right-side y-axis", TRUE),

                      # Space ----
                      tags$br(),

                      # colour
                      h4("Colour settings"),

                      # Plot: colour
                      checkboxInput("greyScale", "Grey Scale", TRUE),
                      actionButton("resetCol", "Reset bar colours", icon = icon("undo")),
                      colourInput("barOutline", "Bar ourline colour", value = "black", returnName = TRUE, palette = "limited"),

                      # Space ----
                      tags$br(),

                      # Legend
                      h4("Legend settings"),

                      # Plot: legend
                      numericInput(inputId = "legendSize", label = "Legend size", value = 9),
                      checkboxInput("legendTtl", "Display legend title", FALSE),
                      numericInput(inputId = "legendTtlSize", label = "Legend title size", value = 9),

                      # Space ----
                      tags$br(),

                      # error bar
                      h4("Error bar settings"),
                      radioButtons("errorbar", "Type", choices = c(SEM = "sem", SD = "sd"),
                                   selected = "sem"),
                      numericInput(inputId = "errorbarWidth", label = "Width", value = 0.1, step = 0.01),
                      numericInput(inputId = "errorbarLblSize", label = "Label size", value = 6, step = 1),
                      numericInput(inputId = "errorbarLblSpace", label = "Space below label", value = 0.07, step = 0.01),

                      # Space ----
                      tags$br(),

                      # Plot: x-axis
                      h4("X-axis settings"),
                      checkboxInput("xTickItalic", "Italic axis ticks", FALSE),
                      textInput("xLabel", "Axis label", value = NULL, width = NULL, placeholder = NULL),
                      numericInput(inputId = "xLabelSize", label = "Axis label size", value = 10),
                      numericInput(inputId = "xTickLblSize", label = "Tick label size", value = 10),
                      numericInput(inputId = "xAngle", label = "Tick label angle", value = 0, step = 15),
                      radioButtons("xAlign", "Tick label alignment", choices = c(`0` = 0, `0.5` = 0.5, `1` = 1),
                                   selected = 0.5),

                      # Space ----
                      tags$br(),

                      # Plot: y-axis
                      h4("Y-axis settings"),
                      checkboxInput("yTickItalic", "Italic axis ticks", FALSE),
                      textInput("yLabel", "Axis label", value = NULL, width = NULL, placeholder = NULL),
                      numericInput(inputId = "yLabelSize", label = "Axis label size", value = 10),
                      numericInput(inputId = "yTickLblSize", label = "Tick label size", value = 10),
                      numericInput(inputId = "y_lower_limit", label = "Axis lower limit", value = 0, step = 0.25),
                      numericInput(inputId = "y_upper_limit", label = "Axis upper limit", value = NULL, step = 0.25),
                      numericInput(inputId = "y_major_tick_range", label = "Major tick range", value = 0.5, step = 0.25),
                      numericInput(inputId = "y_n_minor_ticks", label = "Number of minor ticks", value = 4)
                    ),
                    mainPanel(uiOutput("barCol"),
                              plotOutput("Plot", height = 480, width = 550))
                    )),

                    tabPanel("Plot summary", sidebarLayout(sidebarPanel(
                      h2("Plot summary"),
                      div(style = "display:inline-block", downloadButton("dlSummary", "Save plot summary")),
                      tags$hr(),
                      actionButton("close3", "Close App", icon = icon("exclamation"),
                                   onclick = "setTimeout(function(){window.close();}, 100);")
                    ),
                    mainPanel(tableOutput("Summary"))
                    ))
    ),

    server = function(input, output, session){
      ## input data check
      # input$file1 will be NULL initially.
      data <- reactive({
        req(input$file1)
        df <- read.table(file = input$file1$datapath, header = TRUE, sep = input$sep,
                         na.strings = "NA", stringsAsFactors = FALSE, check.names = FALSE)
        df[[1]] <- factor(df[[1]], levels = c(unique(df[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "typed-in" order

        return(df)
      })

      ## to display raw data
      # After the user selects and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      output$contents <- renderTable({
        c <- length(unique(data()[[1]]))
        validate(need(c >= 2, "Error: \n
                      rbioplot() requires more than one group (e.g. experimental condition).\n
                      Try again."))

        if(input$disp == "head"){
          return(head(data()))
        }
        else {
          return(data())
        }
      })


      ## Plot
      output$barCol <- renderUI({  # colour picker
        lev <- sort(unique(pltdata()$Condition)) # sorting so that "things" are unambigious
        cols <- set_hue(length(lev))

        # New IDs "colX1" so that it partly coincide with input$select...
        lapply(seq_along(lev), function(i) {
          colourInput(inputId = paste0("col", gsub(" ", "", lev[i])), # use gsub to get rid of the spaces for the ID
                      label = paste0("Choose colour for ", lev[i]),
                      value = cols[i]
          )
        })
      })

      observeEvent(input$resetCol, {  # colour reset button
        lev <- sort(unique(pltdata()$Condition)) # sorting so that "things" are unambigious
        cols <- set_hue(length(lev))

        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("col", gsub(" ", "", lev[i])),
                    value = cols[i]
                  )
          )
        })
      })


      pltdata <- reactive({
        # validate
        if (input$Tp == "t-test"){
          validate(need(nlevels(data()[[1]]) == 2, "Error: \n
                        T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try Tukey or Dunnett).\n
                        Try again."))
        } else {
          validate(need(nlevels(data()[[1]]) != 2, "Error: \n
                        USE T-TEST FOR A TWO-GROUP COMPARISON.\n
                        Try again."))
        }

        # calculations for the metrics
        Mean <- sapply(colnames(data())[-1],
                       function(i) tapply(data()[[i]], data()[1], mean, na.rm = TRUE))
        Mean <- data.frame(Mean, check.names = FALSE) # add the check.name argument to preserve the name of the variables. same as all the following data.frame() usage
        Mean$Condition <- factor(rownames(Mean), levels = c(rownames(Mean)))
        if (input$Nrm){ # normalize to control as 1
          MeanNrm <- data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                                       function(i)sapply(Mean[[i]], function(j)j/Mean[[i]][1])),
                                Condition = factor(rownames(Mean), levels = c(rownames(Mean))), check.names = FALSE) # keep the correct factor level order with levels=c().
        } else {
          MeanNrm <- Mean
        }

        # calculations for error bar
        if (input$errorbar == "sem"){

          SEM <- sapply(colnames(data())[-1],
                        function(i) tapply(data()[[i]], data()[1],
                                           function(j)sd(j, na.rm = TRUE)/sqrt(length(!is.na(j)))))
          SEM <- data.frame(SEM, check.names = FALSE)
          SEM$Condition <- factor(rownames(SEM), levels = c(rownames(SEM)))

          if (input$Nrm){
            SEMNrm <- data.frame(sapply(colnames(SEM)[-length(colnames(SEM))],
                                        function(i)sapply(SEM[[i]], function(j)j/Mean[[i]][1])),
                                 Condition = factor(rownames(SEM), levels = c(rownames(SEM))), check.names = FALSE) # keep the correct factor level order with levels=c().
          } else {
            SEMNrm <- SEM
          }

          colnames(SEMNrm)[-length(colnames(SEMNrm))] <- sapply(colnames(data())[-1],
                                                                function(x)paste(x, "SEM", sep = ""))

        } else if (input$errorbar == "sd"){
          SD <- sapply(colnames(data())[-1],
                       function(i) tapply(data()[[i]], data()[1],
                                          function(j)sd(j, na.rm = TRUE)))
          SD <- data.frame(SD, check.names = FALSE)
          SD$Condition <- factor(rownames(SD), levels = c(rownames(SD)))

          if (input$Nrm){
            SDNrm <- data.frame(sapply(colnames(SD)[-length(colnames(SD))],
                                       function(i)sapply(SD[[i]], function(j)j/Mean[[i]][1])),
                                Condition = factor(rownames(SD), levels = c(rownames(SD))), check.names = FALSE)
          } else {
            SDNrm <- SD
          }

          colnames(SDNrm)[-length(colnames(SDNrm))] <- sapply(colnames(data())[-1],
                                                              function(x)paste(x, "SD", sep = ""))
        }

        # for automatic significant labels (Tukey: letters; t-test & Dunnett: asterisks)
        cNm <- colnames(data())
        Tt <- sapply(colnames(data())[-1],
                     function(i) {
                       quoteName <- paste0("`", i, "`", sep = "") # add the single quote to the variable names, to ensure the preservation of the names
                       fml<-paste(quoteName, cNm[1], sep = "~")
                       Mdl<-aov(formula(fml), data = data())

                       if (input$Tp %in% c("t-test", "t test", "ttest", "t")){
                         Control <- subset(data()[i], data()[[1]] == levels(data()[[1]])[1])
                         Experimental <- subset(data()[i], data()[[1]] == levels(data()[[1]])[2])
                         Ttest <- t.test(Control, Experimental, var.equal = TRUE, na.rm = TRUE)
                         Ttestp <- Ttest$p.value
                         Lvl <- data.frame(Condition = unique(data()[[1]]), pvalue = c(1, Ttestp))
                         Lvl$Lbl <- sapply(Lvl$pvalue, function(x)ifelse(x < 0.05, "*", ""))
                         Lvl <- Lvl[,c(1,3)]
                       } else if (input$Tp %in% c("tukey")){
                         Sts <- TukeyHSD(Mdl)
                         Tkp <- Sts[[1]][,4]
                         names(Tkp) <- sapply(names(Tkp), function(j)revsort(j)) # change orders (from b-a to a-b)
                         Tkp <- multcompLetters(Tkp)["Letters"] # from the multcompView package.
                         Lbl <- names(Tkp[["Letters"]])
                         Lvl <- data.frame(Lbl, Tkp[["Letters"]], stringsAsFactors = FALSE)
                       } else if (input$Tp %in% c("dunnett", "dunnett\'s", "dunnetts")){
                         var <- cNm[1]
                         arg <- list("Dunnett")
                         names(arg) <- var
                         mcp <- do.call(mcp, arg)
                         Sts <- summary(glht(Mdl, linfct = mcp))
                         Dnt <- Sts$test$pvalues
                         names(Dnt) <- names(Sts$test$coefficients)
                         Lvl <- data.frame(Condition = unique(data()[[1]]),pvalue = c(1,Dnt))
                         Lvl$Lbl <- sapply(Lvl$pvalue, function(x)ifelse(x < 0.05, "*", ""))
                         Lvl <- Lvl[, c(1, 3)]
                       }

                       colnames(Lvl) <- c(colnames(data())[1], i)
                       Lvl
                     }, simplify = FALSE)

        cTt <- Reduce(function(x, y) merge(x, y, all = TRUE, by = colnames(data())[1], sort = FALSE),
                      Tt, accumulate = FALSE) # Reduce() higher level funtion to contain other fucntions in functional programming
        colnames(cTt)[-1] <- sapply(colnames(data())[-1], function(x)paste(x, "Lbl", sep=""))

        # plot
        # generate the master dataframe for plotting
        MeanNrmMLT <- melt(MeanNrm,id.vars = colnames(MeanNrm)[length(colnames(MeanNrm))]) # melt mean
        MeanNrmMLT$id <- rownames(MeanNrmMLT)

        cTtMLT <- melt(cTt,id.vars = colnames(cTt)[1]) # melt labels
        cTtMLT$id <- rownames(cTtMLT)
        cTtMLT[1] <- as.factor(cTtMLT[[1]])

        colnames(MeanNrmMLT)[3] <- "NrmMean" # give unique variable names
        colnames(cTtMLT)[1:3] <- c(colnames(MeanNrmMLT)[1], "variableLbl", "Lbl") # same as above and make sure to have the same "Condition" variable name for merging

        if (input$errorbar == "sem"){
          SEMNrmMLT <- melt(SEMNrm,id.vars = colnames(SEMNrm)[length(colnames(SEMNrm))])
          SEMNrmMLT$id <- rownames(SEMNrmMLT)
          colnames(SEMNrmMLT)[2:3] <- c("variableSEM", "NrmErr")

          DfPlt <- merge(MeanNrmMLT, SEMNrmMLT, by = c("id", "Condition"), sort = FALSE)
          DfPlt <- merge(DfPlt, cTtMLT, by = c("id", "Condition"), sort = FALSE)
        } else if (input$errorbar == "sd"){
          SDNrmMLT <- melt(SDNrm,id.vars = colnames(SDNrm)[length(colnames(SDNrm))])
          SDNrmMLT$id <- rownames(SDNrmMLT)
          colnames(SDNrmMLT)[2:3] <- c("variableSD", "NrmErr")

          DfPlt <- merge(MeanNrmMLT, SDNrmMLT, by = c("id", "Condition"), sort = FALSE)
          DfPlt <- merge(DfPlt, cTtMLT, by = c("id", "Condition"), sort = FALSE)
        }

        return(DfPlt)
        })

      ggplotdata <- reactive({
        cNm <- colnames(data())

        y_axis_Mx <- with(pltdata(), ceiling((max(NrmMean + NrmErr) + 0.09) / 0.5) * 0.5)
        y_axis_Mn <- input$y_lower_limit
        major_tick_range <- input$y_major_tick_range # determined by the autorange_bar_y() function - major_tick_range
        n_minor_ticks <- input$y_n_minor_ticks # chosen by the autorange_bar_y() function - minor_tick_options

        loclEnv <- environment()
        baseplt <- ggplot(data = pltdata(), aes(x= variable, y= NrmMean, fill = Condition),
                          environment = loclEnv) +
          geom_bar(position = "dodge", stat = "identity", color = input$barOutline) +
          geom_errorbar(aes(ymin = NrmMean - NrmErr, ymax = NrmMean + NrmErr), width = input$errorbarWidth,
                        position = position_dodge(0.9))+
          scale_y_continuous(expand = c(0, 0),
                             breaks = seq(y_axis_Mn, y_axis_Mx, by = major_tick_range / (n_minor_ticks + 1)),  # based on "n_minor_ticks = major_tick_range / minor_tick_range - 1"
                             labels = minor_tick(seq(y_axis_Mn, y_axis_Mx, by = major_tick_range), n_minor_ticks),
                             limits = c(y_axis_Mn, y_axis_Mx), oob = rescale_none)+
          ggtitle(input$Title) +
          xlab(input$xLabel) +
          ylab(input$yLabel) +
          theme(panel.background = element_rect(fill = 'white', colour = 'black'),
                panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
                plot.title = element_text(hjust = 0.5, face = "bold", family = input$fontType, size = input$TitleSize),
                axis.title.x = element_text(face = "bold", family = input$fontType, size = input$xLabelSize),
                axis.title.y = element_text(face = "bold", family = input$fontType, size = input$yLabelSize),
                legend.position = "bottom",
                legend.text = element_text(size = input$legendSize),
                axis.text.x = element_text(size = input$xTickLblSize, family = input$fontType, angle = input$xAngle, hjust = input$xAlign),
                axis.text.y = element_text(size = input$yTickLblSize, family = input$fontType, hjust = 0.5))

        if (input$greyScale){
          baseplt <- baseplt +
            scale_fill_grey(start = 0, name = cNm[1]) # set the colour as gray scale and legend tile as the name of the first column in the raw data.
        } else {
          cols <- paste0("c(", paste0("input$col", gsub(" ", "", sort(unique(pltdata()$Condition))), collapse = ", "), ")")
          cols <- eval(parse(text = cols))

          baseplt <- baseplt +
            scale_fill_manual(values = cols)
        }

        if (input$xTickItalic){
          baseplt <- baseplt +
            theme(axis.text.x = element_text(face = "italic"))
        }

        if (input$yTickItalic){
          baseplt <- baseplt +
            theme(axis.text.y = element_text(face = "italic"))
        }

        if (input$Tp == "Tukey"){
          pltLbl <- baseplt +
            geom_text(aes(y = NrmMean + NrmErr + input$errorbarLblSpace, label = Lbl), position = position_dodge(width = 0.9),
                      color = "black", size = input$errorbarLblSize) # the labels are placed 0.07 (tested optimal for letters) unit higher than the mean + SEM.
        } else {
          pltLbl <- baseplt +
            geom_text(aes(y = NrmMean + NrmErr + input$errorbarLblSpace, label = Lbl), position = position_dodge(width = 0.9),
                      size = input$errorbarLblSize, color = "black") # font size 6 and 0.06 unit higher is good for asterisks.
        }

        if (input$legendTtl){
          pltLbl <- pltLbl + theme(legend.title = element_text(size = input$legendTtlSize))
        } else {
          pltLbl <- pltLbl + theme(legend.title = element_blank())
        }

        if (nlevels(pltdata()$variable) == 1){
          plt <- pltLbl +
            theme(axis.text.x = element_blank()) +
            coord_equal(ratio = 0.5) +
            scale_x_discrete(expand = c(0.1, 0.1)) # space between y axis and fist/last bar
        } else {
          plt <- pltLbl
        }

        ## finalize the plot
        grid.newpage()

        if (input$rightsideY){ # add the right-side y axis
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

        } else { # no right side y-axis
          pltgtb <- plt
        }
        return(pltgtb)
      })

      observe({
        output$Plot <- renderPlot({
          grid.draw(ggplotdata())
        }, height = input$plotHeight, width = input$plotWidth)
      })

      output$dlPlot <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".bar.pdf", sep = "")},
        content = function(file) {
          ggsave(file, plot = ggplotdata(),
                 width = (input$plotWidth * 25.4) / 72, height = (input$plotHeight * 25.4) / 72, units = "mm", dpi = 600, device = "pdf")
        }
      )

      output$dlSummary <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".bar.csv", sep = "")},
        content = function(file){
          write.csv(pltdata(), file, quote = FALSE, na = "NA", row.names = FALSE)
        }
      )

      # summary
      output$Summary <- renderTable({
        return(pltdata())
      })


      # stop and close window
      observe({
        if (input$close > 0) stopApp()  # stop shiny
      })
      observe({
        if (input$close2 > 0) stopApp()  # stop shiny
      })
      observe({
        if (input$close3 > 0) stopApp()  # stop shiny
      })
    }
  )
  runApp(app, launch.browser = TRUE)
}
