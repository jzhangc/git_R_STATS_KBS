#' @title rbiostats
#'
#' @description A simple to use function for comprehensive statistical analyses.
#' @param fileName Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Tp Type of the intended statistical test. e sure to type with quotation marks. Options are: "t-test", "Tukey" and "Dunnett" (Case insensitive). Default is "ANOVA".
#' @return Outputs a \code{.txt} file with Shapiro-Wilk normality test results and the results of the statistical analysis of interest.
#' @importFrom multcomp glht mcp
#' @examples
#' \dontrun{
#' rbiostats("data.csv","Tukey")
#' rbiostats("data2.csv","t-test")
#' }
#' @export
rbiostats <- function(fileName, Tp = "ANOVA"){
  rawData <- read.csv(file = fileName, header = TRUE, na.strings = "NA", stringsAsFactors = FALSE, check.names = FALSE)
  rawData[[1]] <- factor(rawData[[1]], levels = c(unique(rawData[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "type-in" order

  cNm <- colnames(rawData)

  # below: nchar() counts the number of the characters: note the diference between length(),
  # which counts "how many" the *whole* character strings.
  # ALSO, to use substr(), the object has to have "no quote" - use the function noquote() to achieve.
  cat(paste(tolower(Tp), " test results written to file: ", substr(noquote(fileName), 1, nchar(fileName) - 4), ".stats.txt ...", sep = "")) # initial message

  sink(file = paste(substr(noquote(fileName), 1, nchar(fileName) - 4), ".stats.txt", sep = ""), append = FALSE)
  # below: Shapiro-Wilk normality test. p>0.5 means the data is normal.
  print(sapply(cNm[-1],
               function(i)tapply(rawData[[i]], rawData[1], function(x)shapiro.test(x)),
               simplify = FALSE))
  # below: stats
  print(sapply(cNm[-1], function(x){
    quoteName <- paste0("`", x, "`", sep = "")
    fml <- paste(quoteName, cNm[1], sep = "~")
    Mdl <- aov(formula(fml), data = rawData) # fit an analysis of variance model by a call to lm(), applicable for both balanced or unbalanced data set.
    # below: make sure to chain if else in this way!
    if (tolower(Tp) %in% c("t-test", "t test", "ttest", "t")){
      if (nlevels(rawData[[1]]) == 2){
        Control <- subset(rawData[x], rawData[[1]] == levels(rawData[[1]])[1])
        Experimental <- subset(rawData[x], rawData[[1]] == levels(rawData[[1]])[2])
        Eqv <- bartlett.test(formula(fml), data = rawData) # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
        tTest <- t.test(Control, Experimental, var.equal = TRUE ,na.rm = TRUE)
        statsLst <- list(EqualVariance = Eqv, ttest = tTest)
      } else {"T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try ANOVA/Tukey/Dunnett)."}
    } else if (tolower(Tp) %in% c("anova")){
      if (nlevels(rawData[[1]]) > 2){
        Eqv <- bartlett.test(formula(fml), data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
        statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else if (tolower(Tp) %in% c("tukey")){
      if (nlevels(rawData[[1]]) > 2){
        Eqv <- bartlett.test(formula(fml), data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
        statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl), Tukey = TukeyHSD(Mdl))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else if (tolower(Tp) %in% c("dunnett", "dunnett\'s", "dunnetts")){
      if (nlevels(rawData[[1]]) > 2){
        Eqv <- bartlett.test(formula(fml), data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups is equal.
        var <- cNm[1]
        arg <- list("Dunnett")
        names(arg) <- var
        mcp <- do.call(mcp, arg)
        statsLst <- list(EqualVariance = Eqv, ANOVA = anova(Mdl), Dunnett = summary(glht(Mdl, linfct = mcp)))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else {
      "ERROR: CHECK YOUR STATS TEST."
    }
  },  simplify = FALSE)
  )
  sink() # end the dump

  cat("Done!\n") # final message
}


#' @title rbiostats_normal
#'
#' @description A simple to use function for Shapiro-Wilk test for normality.
#' @param x Input data frame or matrix for test data.
#' @param method Shaprio-Wilk test method. Options are \code{"residual"} and \code{"group"}. Default is \code{"residual"}.
#' @param group Set only when \code{method = "group"}, a factor vector for the sample data groupping.
#' @return Outputs a data frame object containing normality test results to the environment.
#' @details When \code{method = "residual"}, the residual for each data point is calculated using \code{residual(i) = x(i) - mean(X)} before Shapiro_Wilk test.
#' In such case, the test is done on all the data points regardless of groupping. Regarding \code{method = "group"}, the raw data is used. However, the test is done
#' according to the groupping.
#' @import foreach
#' @examples
#' \dontrun{
#'
#' rbiostats_normal(x = data, method = "group", group = group)
#'
#' }
#' @export
rbiostats_normal <- function(x, method = c("residual", "group"), group){
  ## arugment check
  if (missing(x)) stop("Input data missing.")
  if (is.null(group)) stop("Please provide value for \"group\" valiable.")
  if (!method %in% c("residual", "group")) stop("Invalid method. Please use \"residual\" or \"group\".")
  if (!class(x) %in% c("matrix", "data.frame")) stop("x needs to be either a matrix or a data frame.")

  ## pre-processing x
  if (is.null(colnames)) {
    cat("No column names found for x. Use column numbers as variable names. \n")
    colnames(x) <- seq(ncol(x))
    }

  ## normality test
  if (missing(group) | method == "residual"){
    # set up data
    test_dfm <- as.data.frame(x)

    # normality test
    cat("Testing data normality by residuals using Shapiro-Wilk test...")
    normal_test_results <- foreach(i = 1:ncol(test_dfm), .combine = "rbind") %do% {
      tmp <- test_dfm[, i] - mean(test_dfm[, i], na.rm = TRUE) # residual calculation
      sp <- shapiro.test(tmp)
      sp$data.name <- colnames(test_dfm)[i]
      sp_dfm <- data.frame(Variable = sp$data.name, W.statistic = sp$statistic, p.value = signif(sp$p.value, 4), row.names = NULL,
                           check.names = FALSE)
      sp_dfm$Normal <- ifelse(sp_dfm$p.value < 0.05, "N", "Y")
      sp_dfm
    }
  } else {
    # check group argument
    if (!is.null(dim(group))) stop("the group vector needs to be a vector.")

    # set up data
    if (class(group) != "factor"){
      y <- factor(group, levels = unique(group))
    } else {
      y <- group
    }
    test_dfm <- data.frame(group = y, x)

    # normality test
    cat("Testing data normality according to grouping using Shapiro-Wilk test...")
    normal_test_results <- foreach(i = 1:ncol(test_dfm[, -1, drop = FALSE]), .combine = "rbind") %do% {
      out <- foreach(j = 1:length(levels(test_dfm$group)), .combine = "rbind") %do% {
        sp <- shapiro.test(test_dfm[, -1, drop = FALSE][, j][test_dfm$group %in% levels(test_dfm$group)[j]])
        sp$data.name <- colnames(ric[, c(11:20)])[i]
        sp_dfm <- data.frame(Variable = sp$data.name, Conditions = levels(test_dfm$group)[j],
                             W.statistic = sp$statistic, p.value = signif(sp$p.value, 4), row.names = NULL, check.names = FALSE)
        sp_dfm$Normal <- ifelse(sp_dfm$p.value < 0.05, "N", "Y")
        sp_dfm
      }
      out
    }
  }

  ## output
  cat("Done!\n")
  cat(paste0("Number of variables ", ifelse(method == "residual", "(as per residual) ","(as per group) "),
             "failed to pass normality test: ",
             length(which(normal_test_results$Normal == "N")), ".\n"))
  cat("\n")
  cat("Results: \n")
  print(normal_test_results)

  out <- list(method = paste0("Shapiro-Wilk Nomality based on ", method), `failed to pass` = length(which(normal_test_results$Normal == "N")),
              results = normal_test_results)
  class(out) <- "rbiostats_normality"
  assign(paste0(deparse(substitute(x)), "_normality"), out, envir = .GlobalEnv)
}
