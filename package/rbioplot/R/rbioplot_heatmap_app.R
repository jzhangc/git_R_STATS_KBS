#' @title rbioplot_heatmap_app
#'
#' @description The web app version of \code{\link{rbioplot_heatmap}}.
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
#' rbioplot_heatmap_app() # launch the app version by running the function
#' }
#' @export
rbioplot_heatmap_app <- function(){
  shinyApp(
    ui = fluidPage(
      ## App title ----
      titlePanel(h1("Function: rbioplot_heatmap()")),

      ## Sidebar layout with input and output definitions ----
      sidebarLayout(

        ## Sidebar panel for inputs ----
        sidebarPanel(
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
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          # Horizontal line ----
          tags$hr(),

          ## Input block
          h2("Input file settings"),

          # Input: Select separator ----
          radioButtons("sep",
                       "Separator",
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","), # selected = "," term sets the default value

          # Input: Select number of rows to display ----
          radioButtons("disp", "Display", choices = c(Head = "head", All = "all"),
                       selected = "head"),

          # Horizontal line ----
          tags$hr(),

          ## Quick plot
          h2("Plot"),

          # plot: stats
          radioButtons("Tp", "Statistical anlaysis", choices = c(`t-test` = "t-test", `ANOVA + Tukey` = "tukey", `ANOVA + Dunnett\'s` = "dunnetts"),
                       selected = "t-test"),

          # Buttons
          div(style = "display:inline-block", downloadButton("dlPlot", "Save plot")),
          div(style = "display:inline-block", downloadButton("dlSummary", "Save plot summary")),

          # Horizontal line ----
          tags$hr(),

          ## Plot settings
          h2("Detailed plot settings"),

          # Plot: title
          textInput("Title", "Plot title", value = NULL, width = NULL, placeholder = NULL),

          # Plot: font
          textInput("fontType", "Font type", value = "sans", width = NULL, placeholder = NULL),
          actionButton(inputId = "fontTable", "Font table", icon = icon("th"), onclick = "window.open('http://kenstoreylab.com/wp-content/uploads/2015/08/R-font-table.png', '_blank')"),

          ## Plot settings
          h2("Detailed plot settings"),

          # Plot: size
          numericInput(inputId = "plotWidth", label = "Plot width",
                       value = 800, step = 10),
          numericInput(inputId = "plotHeight", label = "Plot height",
                       value = 600, step = 10),

          # Plot: if to remove control
          checkboxInput("rmCntl", "Remove control", FALSE),

          # Plot: legend title
          checkboxInput("legendTtl", "Display legend title", FALSE),
          radioButtons("legendPos", "Legend position", choices = c(none = "none", left = "left", right = "right", bottom = "bottom", top = "top"),
                       selected = "bottom"),

          # Space ----
          tags$br(),

          # Plot: tiles
          h2("Tile settings"),
          # tile colour code
          colourInput("tileLow", "Lower end colour", value = "skyblue", returnName = TRUE, palette = "square"),
          colourInput("tileHigh", "Higher end colour", value = "midnightblue", returnName = TRUE, palette = "square"),
          checkboxInput("tileLbl", "Display tile label", TRUE),
          numericInput(inputId = "tileLblSize", label = "Label size", value = 10, step = 1),
          colourInput("tileTxtColour", "Label colour", value = "white", returnName = TRUE, palette = "limited"),
          radioButtons("tileLblPos", "Label position", choices = c(`0` = 0, `0.5` = 0.5, `1` = 1),
                       selected = 0.5),

          # Space ----
          tags$br(),

          # Plot: x-axis
          h4("X-axis"),
          checkboxInput("xTickItalic", "Italic axis ticks", FALSE),
          textInput("xLabel", "Axis label", value = NULL, width = NULL, placeholder = NULL),
          numericInput(inputId = "xTickLblSize", label = "Tick label size",
                       value = 10),
          numericInput(inputId = "xAngle", label = "Tick label angle",
                       value = 0, step = 15),
          numericInput(inputId = "xSpace", label = "Tick label space",
                       value = 5, step = 1),
          radioButtons("xAlign", "Tick label alignment", choices = c(`0` = 0, `0.5` = 0.5, `1` = 1),
                       selected = 0.5),

          # Space ----
          tags$br(),

          # Plot: y-axis
          h4("Y-axis"),
          checkboxInput("yTickItalic", "Italic axis ticks", FALSE),
          textInput("yLabel", "Axis label", value = NULL, width = NULL, placeholder = NULL),
          numericInput(inputId = "yTickLblSize", label = "Tick label size",
                       value = 10)
        ),

        ## Main panel for displaying outputs ----
        mainPanel(
          # set up tabs
          tabsetPanel(type = "tabs",
                      tabPanel("Raw data", tableOutput("contents")), # "contents" means go to output to find the variable output$contents
                      tabPanel("Plot", plotOutput("Plot", height = 480, width = 550)),
                      tabPanel("Plot Summary", tableOutput("Summary")))
        ), fluid = FALSE
      )
    ),

    server = function(input, output){

      ## input data check
      # input$file1 will be NULL initially.
      data <- reactive({
        req(input$file1)
        df <- read.table(file = input$file1$datapath, header = TRUE, sep = input$sep,
                         na.strings = "NA", stringsAsFactors = FALSE,
                         check.names = FALSE)
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
        MeanNrm <- data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                                     function(i)sapply(Mean[[i]], function(j)j/Mean[[i]][1])),
                              Condition = factor(rownames(Mean), levels = c(rownames(Mean))), check.names = FALSE) # keep the correct factor level order with levels=c().

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

        colnames(MeanNrmMLT)[3] <- "Fold.Change"
        colnames(cTtMLT)[1:3] <- c(colnames(MeanNrmMLT)[1], "variableLbl", "Lbl")

        DfPlt <- merge(MeanNrmMLT, cTtMLT, by = c("id", "Condition"), sort = FALSE)

        if (input$rmCntl == TRUE){
          DfPlt <- DfPlt[DfPlt$Condition %in% levels(DfPlt$Condition)[-1], ]
          DfPlt$Condition <- factor(DfPlt$Condition, levels = unique(DfPlt$Condition))
        }

        return(DfPlt)
        })

      ggplotdata <- reactive({
        cNm <- colnames(data())

        loclEnv <- environment()
        baseplt <- ggplot(data = pltdata(), aes(x = Condition, y = variable),
                          environment = loclEnv) +
          geom_tile(aes(fill = Fold.Change), colour = "white") +
          scale_fill_gradient(low = input$tileLow, high = input$tileHigh,
                              breaks = c(ceiling(with(pltdata(), min(Fold.Change) / 0.05)) * 0.05, floor(with(pltdata(), max(Fold.Change)) / 0.05) * 0.05),
                              guide = guide_colorbar(ticks = FALSE)) +
          scale_y_discrete(expand = c(0, 0)) +
          scale_x_discrete(expand = c(0, 0)) +
          ggtitle(input$Title) +
          xlab(input$xLabel) +
          ylab(input$yLabel) +
          theme(panel.background = element_rect(fill = 'white', colour = 'black'),
                panel.border = element_blank(),
                axis.ticks = element_line(colour = "white", size = 0),
                plot.title = element_text(face = "bold", family = input$fontType),
                axis.title = element_text(face = "bold", family = input$fontType),
                legend.position = input$legendPos,
                axis.text.x = element_text(size = input$xTickLblSize, family = input$fontType, angle = input$xAngle, hjust = input$xAlign,
                                           margin = margin(t = 5, r = 5, b = input$xSpace, l = 5, unit = "pt")),
                axis.text.y = element_text(size = input$yTickLblSize, family = input$fontType, hjust = 0.5))

        if (input$tileLbl){
          baseplt <- baseplt +
            geom_text(aes(x = Condition, y = variable, label = Lbl), size = input$tileLblSize, vjust = input$tileLblPos,
                      color = input$tileTxtColour, family = input$fontType)
        }

        if (input$xTickItalic == TRUE){
          baseplt <- baseplt +
            theme(axis.text.x = element_text(face = "italic"))
        }

        if (input$yTickItalic == TRUE){
          baseplt <- baseplt +
            theme(axis.text.y = element_text(face = "italic"))
        }

        if (input$legendTtl == FALSE){
          pltLbl <- baseplt + theme(legend.title = element_blank())
        } else {
          pltLbl <- baseplt + theme(legend.title = element_text(size = 9))
        }

        if (nlevels(pltdata()$variable) == 1){
          plt <- pltLbl +
            theme(axis.text.x = element_blank()) +
            coord_equal(ratio = 0.5) +
            scale_x_discrete(expand = c(0.1, 0.1)) # space between y axis and fist/last bar
        } else {
          plt <- pltLbl
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

        return(pltgtb)
      })

      observe({
        output$Plot <- renderPlot({
          grid.draw(ggplotdata())
        }, height = input$plotHeight, width = input$plotWidth)
      })

      output$dlPlot <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".histogram.pdf", sep = "")},
        content = function(file) {
          ggsave(file, plot = ggplotdata(),
                 width = (input$plotWidth * 25.4) / 72, height = (input$plotHeight * 25.4) / 72, units = "mm", dpi = 600, device = "pdf")
        }
      )

      output$dlSummary <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".histogram.csv", sep = "")},
        content = function(file){
          write.csv(pltdata(), file, quote = FALSE, na = "NA", row.names = FALSE)
        }
      )

      # summary
      output$Summary <- renderTable({
        return(pltdata())
      })
    }
  )
}
