#' @title rbiostats_app
#'
#' @description The web app version of \code{\link{rbiostats}}.
#' @importFrom multcomp glht mcp
#' @import shiny
#' @examples
#' \dontrun{
#' rbiostats_app() # launch the app version by running the function
#' }
#' @export
rbiostats_app <- function(){
  app <- shinyApp(
    ui = navbarPage(inverse = TRUE,
                    title = HTML("<a style = color:white; href = \"http://http://kenstoreylab.com/?page_id=2448\" target = \"_blank\">FUNCTION: rbiostats</a>"),
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

                    tabPanel("Stats summary", sidebarLayout(sidebarPanel(
                      h2("Stats summary"),
                      # plot: stats
                      radioButtons("Tp", "Statistical anlaysis", choices = c(`t-test` = "t-test", ANOVA = "anova", `ANOVA + Tukey` = "tukey", `ANOVA + Dunnett\'s` = "dunnetts"),
                                   selected = "t-test"),
                      div(style = "display:inline-block", downloadButton("dlSummary", "Save stats summary")),

                      # Horizontal line ----
                      tags$hr(),
                      actionButton("close2", "Close App", icon = icon("exclamation"),
                                   onclick = "setTimeout(function(){window.close();}, 100);")
                    ),
                    mainPanel(verbatimTextOutput("Summary"))
                    ))
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

      ## stats
      mainstatsdata <- reactive({
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

        # for automatic significant labels (Tukey: letters; t-test & Dunnett: asterisks)
        cNm <- colnames(data())

        checkStats <- sapply(cNm[-1],
                             function(i)tapply(data()[[i]], data()[1], function(x)shapiro.test(x)),
                             simplify = FALSE)

        mainStats <- sapply(cNm[-1], function(x){
          quoteName <- paste0("`", x, "`", sep = "")
          fml <- paste(quoteName, cNm[1], sep = "~")
          Mdl <- aov(formula(fml), data = data()) # fit an analysis of variance model by a call to lm(), applicable for both balanced or unbalanced data set.
          # below: make sure to chain if else in this way!
          if (input$Tp == "t-test"){
            Control <- subset(data()[x], data()[[1]] == levels(data()[[1]])[1])
            Experimental <- subset(data()[x], data()[[1]] == levels(data()[[1]])[2])
            Eqv <- bartlett.test(formula(fml), data = data()) # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
            tTest <- t.test(Control, Experimental, var.equal = TRUE ,na.rm = TRUE)
            statsLst <- list(EqualVariance = Eqv, ttest = tTest)
          } else if (input$Tp == "anova"){
            Eqv <- bartlett.test(formula(fml), data = data())  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
            statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl))
          } else if (input$Tp == "tukey"){
            Eqv <- bartlett.test(formula(fml), data = data())  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
            statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl), Tukey = TukeyHSD(Mdl))
          } else if (input$Tp == "dunnetts"){
            Eqv <- bartlett.test(formula(fml), data = data())  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
            var <- cNm[1]
            arg <- list("Dunnett")
            names(arg) <- var
            mcp <- do.call(multcomp::mcp, arg)
            statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl), Dunnett = summary(glht(Mdl, linfct = mcp)))
          }
          return(statsLst)
        },  simplify = FALSE)

        statsOut <- list(Shapiro = checkStats, `main stats` = mainStats)

        return(statsOut)
        })

      output$dlSummary <- downloadHandler(
        filename = function(){paste(substr(noquote(input$file1), 1, nchar(input$file1) - 4),".stats.txt", sep = "")},
        content = function(file){
          sink(file, append = FALSE)
          print(mainstatsdata())
          sink() # end the dump
        }
      )

      # summary
      output$Summary <- renderPrint(
        mainstatsdata()
      )

      # stop and close window
      observe({
        if (input$close > 0) stopApp()  # stop shiny
      })
      observe({
        if (input$close2 > 0) stopApp()  # stop shiny
      })
    }
  )
  runApp(app, launch.browser = TRUE)
}
