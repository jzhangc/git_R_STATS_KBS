#' @title rbioplot_curve_app
#'
#' @description The web app version of \code{\link{rbioplot_curve}}.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import shiny
#' @examples
#' \dontrun{
#' rbioplot_curve_app() # launch the app version by running the function
#' }
#' @export
rbioplot_curve_app <- function(){
  app <- shinyApp(

    ui = fluidPage(
      ## App title ----
      titlePanel(h1("Function: rbioplot_curve()")),

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

          # Buttons
          div(style = "display:inline-block", downloadButton("dlPlot", "Save plot")),
          div(style = "display:inline-block", downloadButton("dlSummary", "Save plot summary")),

          # Horizontal line ----
          tags$hr(),

          # exit
          actionButton("close", "Close App", icon = icon("exclamation"),
                       onclick = "setTimeout(function(){window.close();}, 100);"),

          # Horizontal line ----
          tags$hr(),

          ## Plot settings
          h2("Detailed plot settings"),

          # Plot: title
          textInput("Title", "Plot title", value = NULL, width = NULL, placeholder = NULL),

          # Plot: font
          textInput("fontType", "Font type", value = "sans", width = NULL, placeholder = NULL),
          actionButton(inputId = "fontTable", "Font table", icon = icon("th"), onclick = "window.open('http://kenstoreylab.com/wp-content/uploads/2015/08/R-font-table.png', '_blank')"),

          # Plot: symbol size
          numericInput(inputId = "symbolSize", label = "Symbol size",
                       value = 2, step = 1),

          # Plot: size
          numericInput(inputId = "plotWidth", label = "Plot width",
                       value = 800, step = 10),
          numericInput(inputId = "plotHeight", label = "Plot height",
                       value = 600, step = 10),

          # Plot: legend title
          checkboxInput("legendTtl", "Display legend title", TRUE),

          # Plot: right side y
          checkboxInput("rightsideY", "Display right-side y-axis", TRUE),

          # Space ----
          tags$br(),

          # error bar
          h4("Error bar (if applicable)"),
          radioButtons("errorbar", "Type", choices = c(SEM = "sem", SD = "sd"),
                       selected = "sem"),
          numericInput(inputId = "errorbarWidth", label = "Width",
                       value = 0.1, step = 0.01),

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
          radioButtons("xAlign", "Tick label alignment", choices = c(`0` = 0, `0.5` = 0.5, `1` = 1),
                       selected = 0.5),
          numericInput(inputId = "x_lower_limit", label = "Axis lower limit",
                       value = 0, step = 0.25),
          numericInput(inputId = "x_upper_limit", label = "Axis upper limit",
                       value = NULL, step = 0.25),
          numericInput(inputId = "x_major_tick_range", label = "Major tick range",
                       value = 0.5, step = 0.25),
          numericInput(inputId = "x_n_minor_ticks", label = "Number of miner ticks",
                       value = 0),

          # Space ----
          tags$br(),

          # Plot: y-axis
          h4("Y-axis"),
          checkboxInput("yTickItalic", "Italic axis ticks", FALSE),
          textInput("yLabel", "Axis label", value = NULL, width = NULL, placeholder = NULL),
          numericInput(inputId = "yTickLblSize", label = "Tick label size",
                       value = 10),
          numericInput(inputId = "y_lower_limit", label = "Axis lower limit",
                       value = 0, step = 0.25),
          numericInput(inputId = "y_upper_limit", label = "Axis upper limit",
                       value = NULL, step = 0.25),
          numericInput(inputId = "y_major_tick_range", label = "Major tick range",
                       value = 0.5, step = 0.25),
          numericInput(inputId = "y_n_minor_ticks", label = "Number of miner ticks",
                       value = 4)
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
        # calculations for the metrics
        cNm <- colnames(data()) # load all the column names

        ## calculate mean and SEM
        Mean <- sapply(colnames(data())[-1],
                       function(i)tapply(data()[[i]], data()[1], mean, na.rm = TRUE))
        Mean <- data.frame(Mean)
        Mean$Condition <- factor(rownames(Mean),levels = c(rownames(Mean)))

        if (input$errorbar == "sem"){

          SEM <- sapply(colnames(data())[-1],
                        function(i)tapply(data()[[i]], data()[1],
                                          function(j)sd(j, na.rm = TRUE)/sqrt(length(!is.na(j)))))
          SEM <- data.frame(SEM)
          SEM$Condition <- factor(rownames(SEM), levels = c(rownames(SEM)))
          colnames(SEM)[-length(colnames(SEM))] <- sapply(colnames(data())[-1],
                                                          function(x)paste(x, "SEM", sep = ""))

        } else if (input$errorbar == "sd"){
          SD <- sapply(colnames(data())[-1],
                       function(i)tapply(data()[[i]], data()[1],
                                         function(j)sd(j, na.rm = TRUE)))
          SD <- data.frame(SD)
          SD$Condition <- factor(rownames(SD), levels = c(rownames(SD)))
          colnames(SD)[-length(colnames(SD))] <- sapply(colnames(data())[-1],
                                                        function(x)paste(x, "SD", sep = ""))

        } else {stop("Please properly specify the error bar type, SEM or SD")}

        ## generate the master dataframe for plotting
        MeanMLT <- melt(Mean, id.vars = colnames(Mean)[length(colnames(Mean))])
        MeanMLT$id <- rownames(MeanMLT)
        colnames(MeanMLT)[3] <- "plotMean"

        if (input$errorbar == "sem"){
          SEMMLT <- melt(SEM, id.vars = colnames(SEM)[length(colnames(SEM))])
          SEMMLT$id <- rownames(SEMMLT)
          colnames(SEMMLT)[2:3] <- c("variableSEM", "plotErr")

          DfPlt <- merge(MeanMLT, SEMMLT,by = c("id","Condition"), sort = FALSE)

          DfPlt$variable <- as.character(DfPlt$variable)
          DfPlt$variable <- sapply(DfPlt$variable, function(x)substr(x, 2, nchar(x)))
          DfPlt$variable <- as.numeric(DfPlt$variable)
        } else if (input$errorbar == "sd"){
          SDMLT <- melt(SD, id.vars = colnames(SD)[length(colnames(SD))])
          SDMLT$id <- rownames(SDMLT)
          colnames(SDMLT)[2:3] <- c("variableSD", "plotErr")

          DfPlt<-merge(MeanMLT,SDMLT,by = c("id","Condition"),sort=FALSE)

          DfPlt$variable<-as.character(DfPlt$variable)
          DfPlt$variable<-sapply(DfPlt$variable,function(x)substr(x,2,nchar(x)))
          DfPlt$variable<-as.numeric(DfPlt$variable)
        }

        return(DfPlt)
      })

      ggplotdata <- reactive({
        cNm <- colnames(data())

        # x axis
        x_axis_Mn <- input$x_lower_limit
        x_mj_range <- input$x_major_tick_range
        x_n_mnr <- input$x_n_minor_ticks
        if (is.na(input$x_upper_limit)){
          x_axis_Mx <- with(pltdata(), ceiling((max(unique(variable)) + 0.02) / 0.5) * 0.5)# the default x axis upper limit=max(mean+extra)
        } else {
          x_axis_Mx <- input$x_upper_limit
        }

        # y axis
        y_axis_Mn <- input$y_lower_limit
        y_mj_range <- input$y_major_tick_range
        y_n_mnr <- input$y_n_minor_ticks
        if (is.na(input$y_upper_limit)){
          y_axis_Mx <- with(pltdata(), ceiling((max(plotMean + ifelse(is.na(plotErr), 0, plotErr)) + 0.02) / 0.5) * 0.5) # the default y axis upper limit = max(mean + SEM + label + extra)
        } else {
          y_axis_Mx <- input$y_upper_limit
        }

        loclEnv <- environment()
        baseplt <- ggplot(data = pltdata(), aes(x = variable, y = plotMean, shape = Condition, linetype = Condition),
                          environment = loclEnv) +
          geom_line() +
          geom_point(size = input$symbolSize)+
          geom_errorbar(aes(ymin = plotMean - ifelse(is.na(plotErr), 0, plotErr),
                            ymax = plotMean + ifelse(is.na(plotErr), 0, plotErr)), width = input$errorbarWidth,
                        linetype = "solid") +
          scale_x_continuous(expand = c(0,0),
                             breaks = seq(x_axis_Mn, x_axis_Mx, by = x_mj_range / (x_n_mnr + 1)),
                             labels = minor_tick(seq(x_axis_Mn, x_axis_Mx, by = x_mj_range), x_n_mnr),
                             limits = c(x_axis_Mn, x_axis_Mx)) +
          scale_y_continuous(expand = c(0, 0),
                             breaks = seq(y_axis_Mn, y_axis_Mx, by = y_mj_range / (y_n_mnr + 1)),
                             labels = minor_tick(seq(y_axis_Mn, y_axis_Mx, by = y_mj_range), y_n_mnr),
                             limits = c(y_axis_Mn, y_axis_Mx)) +
          ggtitle(input$Title) +
          xlab(input$xLabel) +
          ylab(input$yLabel) +
          theme(panel.background = element_rect(fill = 'white', colour = 'black'),
                panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
                plot.title = element_text(face = "bold", family = input$fontType),
                axis.title = element_text(face = "bold", family = input$fontType),
                legend.position = "bottom", legend.title = element_blank(),legend.key = element_blank(),
                axis.text.x = element_text(size = input$xTickLblSize, family = input$fontType, angle = input$xAngle, hjust = input$xAlign),
                axis.text.y = element_text(size = input$yTickLblSize, family = input$fontType, hjust = 0.5)) +
          scale_shape_manual(name = cNm[1], values = c(5:(5 + length(unique(pltdata()$Condition))))) +
          scale_linetype_manual(name = cNm[1],values = c(1:(1 + length(unique(pltdata()$Condition)))))

        if (input$xTickItalic){
          baseplt <- baseplt +
            theme(axis.text.x = element_text(face = "italic"))
        }

        if (input$yTickItalic){
          baseplt <- baseplt +
            theme(axis.text.y = element_text(face = "italic"))
        }

        if (input$legendTtl){
          plt <- baseplt + theme(legend.title = element_text(size = 9))
        } else {
          plt <- baseplt + theme(legend.title = element_blank())
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
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".curve.pdf", sep = "")},
        content = function(file) {
          ggsave(file, plot = ggplotdata(),
                 width = (input$plotWidth * 25.4) / 72, height = (input$plotHeight * 25.4) / 72, units = "mm", dpi = 600, device = "pdf")
        }
      )

      output$dlSummary <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".curve.csv", sep = "")},
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
    }
  )
  runApp(app, launch.browser = TRUE)
}
