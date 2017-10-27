library(reshape2)
library(ggplot2)
library(multcompView)
library(multcomp)
library(grid)
library(gtable)
library(scales)
library(shiny)

fluidPage(
  ## App title ----
  titlePanel(h1("Function: rbioplot()")),

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

      # Plot: size
      numericInput(inputId = "plotWidth", label = "Plot width",
                   value = 800, step = 10),
      numericInput(inputId = "plotHeight", label = "Plot height",
                   value = 600, step = 10),

      # Plot: if to normalized to 1
      checkboxInput("Nrm", "Normalize to control as 1", TRUE),

      # Plot: legend title
      checkboxInput("legendTtl", "Display legend title", FALSE),

      # Plot: right side y
      checkboxInput("rightsideY", "Display right-side y-axis", TRUE),

      # Space ----
      tags$br(),

      # error bar
      h4("Error bar"),
      radioButtons("errorbar", "Type", choices = c(SEM = "sem", SD = "sd"),
                   selected = "sem"),
      numericInput(inputId = "errorbarWidth", label = "Width",
                   value = 0.1, step = 0.01),
      numericInput(inputId = "errorbarLblSize", label = "Label size",
                   value = 6, step = 1),
      numericInput(inputId = "errorbarLblSpace", label = "Space below label",
                   value = 0.07, step = 0.01),

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
      numericInput(inputId = "y_n_minor_ticks", label = "Number of minor ticks",
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
)
