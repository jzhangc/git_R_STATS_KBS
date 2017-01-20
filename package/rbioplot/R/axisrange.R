#' @title nat_dvsr
#'
#' @description A simple to use function to find all divisors for an integer number.
#' @param x An integer number
#' @return A vector containing all the divisors for the input integer
#' @examples
#' \dontrun{
#' nat_dvsr(10)
#' }
#' @export
nat_dvsr <- function(x){
tstSeq <- seq_len(abs(x))
tstSeq[x %% tstSeq == 0]
}

#' @title all_dvsr
#'
#' @description A function to find all divisors for any types of number.
#' @param x The number of interest
#' @param i Decimal scaling factor. If the input number is an integer, the starting number will be \code{x*10^i}; if a decimal, the starting number will be \code{x*10^n*10^i} (\code{x*10^(n+i)}), where the auto-determined \code{n} is the decimal amplification factor that transforms the decimal numbers to their corresponding minimum intergers by \code{10*n}. Default is \code{0}.
#' @return A vector containing all the divisors for the input number
#' @examples
#' \dontrun{
#' all_dvsr(0.5)
#' all_dvsr(0.6,1) # with decimal scaling factor set at 1, meaning the divisor will be based on 60.
#' all_dvsr(2)
#' all_dvsr(2,1) # with decimal scaling factor set at 1, meaning the divisor will be based on 20.
#' }
#' @export
all_dvsr <- function(x, i = 0){
  dopt <- getOption("scipen") # get the user setting for scipen
  on.exit(options(scipen = dopt)) # revert back to user scipen setting on exit

  options(scipen = 999) # disable scientific notation
  if (x %% 1 == 0){ # to test integer
    smpl<-x * 10^i # scale up depending on the decimal scale factor
    return(nat_dvsr(smpl) / 10^i) # also scale down if the decimal scaling factor is not default (default=0)
  } else {
    n<-nchar(unlist(strsplit(as.character(x), "[.]"))[2]) # use unlist to unlist the list produced by strsplit

    # Below:
    # IMPORTANT! this converts the "not-real-integer" number into integer with correction of incorrect rounding down.
    sc <- as.integer(round(x * 10^(n + i))) # i is scaleing factor.
    sc <- abs(sc)
    return(nat_dvsr(sc) / (10^(n + i)))
  }
}

#' @title autorange_bar_y
#'
#' @description A function to get custom lower/upper limit, major tick range, as well as minor tick options for y axis, based on a user-defined major tick number.
#' @param fileName Input file name. Data should be arranged same as the input file for \code{\link{rbioplot}}.Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Nrm When \code{TRUE}, normalize data to control/first group (as 1). Default is \code{TRUE}.
#' @param errorbar Set the type of errorbar. Options are standard error of mean (\code{"SEM"}), or standard deviation (\code{"SD"}). Default is \code{"SEM"}.
#' @param nMajorTicks Number of major ticks intended to use for the plot. Note that the input number should be major tick number EXCLUDING 0 (or y axis lower limit if not using 0). Default is \code{5}. Note: Depending on the raw range, the last label may or may not show up due to plotting optimization, see \code{\link{rbioplot}}.
#' @param DfltZero When \code{TRUE}, start y axis from \code{0}. Default is \code{TRUE}.
#' @importFrom reshape2 melt
#' @return A list object containing \code{lower_limit}, \code{upper_limit}, \code{major_tick_range} and \code{minor_tick_options}.
#' @examples
#' \dontrun{
#' autorange_bar_y("data.csv", Nrm = T, errorbar = "SEM", nMajorTicks = 8, DfltZero=FALSE)
#' }
#' @export
autorange_bar_y <- function(fileName, Nrm = TRUE,
                          errorbar = "SEM", nMajorTicks = 5, DfltZero = TRUE){

  ## load file
  rawData <- read.csv(file = fileName, header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]], levels = c(unique(rawData[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "typed-in" order

  if (Nrm == TRUE){
    ## normalize everything to control as 1
    Mean <- sapply(colnames(rawData)[-1],
                 function(i) tapply(rawData[[i]], rawData[1], mean, na.rm = TRUE))
    Mean <- data.frame(Mean)
    Mean$Condition <- factor(rownames(Mean),levels = c(rownames(Mean)))
    MeanNrm <- data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                               function(i)sapply(Mean[[i]], function(j)j / Mean[[i]][1])),
                        Condition = factor(rownames(Mean), levels = c(rownames(Mean)))) # keep the correct factor level order with levels = c(). same below

    if (errorbar == "SEM"){
      SEM <- sapply(colnames(rawData)[-1],
                  function(i) tapply(rawData[[i]], rawData[1],
                                     function(j)sd(j, na.rm = TRUE)/sqrt(length(!is.na(j)))))
      SEM <- data.frame(SEM)
      SEM$Condition <- factor(rownames(SEM), levels = c(rownames(SEM)))
      SEMNrm <- data.frame(sapply(colnames(SEM)[-length(colnames(SEM))],
                                function(i)sapply(SEM[[i]], function(j)j / Mean[[i]][1])),
                         Condition = factor(rownames(SEM), levels = c(rownames(SEM))))
      colnames(SEMNrm)[-length(colnames(SEMNrm))] <- sapply(colnames(rawData)[-1],
                                                          function(x)paste(x, "SEM", sep = ""))
    } else if (errorbar == "SD") {
      SD <- sapply(colnames(rawData)[-1],
                   function(i) tapply(rawData[[i]], rawData[1],
                                      function(j)sd(j, na.rm = TRUE)))
      SD <- data.frame(SD)
      SD$Condition <- factor(rownames(SD), levels = c(rownames(SD)))
      SDNrm <- data.frame(sapply(colnames(SD)[-length(colnames(SD))],
                                 function(i)sapply(SD[[i]], function(j)j / Mean[[i]][1])),
                          Condition = factor(rownames(SD), levels = c(rownames(SD))))
      colnames(SDNrm)[-length(colnames(SDNrm))] <- sapply(colnames(rawData)[-1],
                                                          function(x)paste(x, "SD", sep = ""))

    } else {stop("Please properly specify the error bar type, SEM or SD")}


    ## generate the master dataframe
    MeanNrmMLT <- melt(MeanNrm, id.vars = colnames(MeanNrm)[length(colnames(MeanNrm))])
    MeanNrmMLT$id <- rownames(MeanNrmMLT)
    colnames(MeanNrmMLT)[3] <- "plotMean"

    if (errorbar == "SEM"){
      SEMNrmMLT <- melt(SEMNrm, id.vars = colnames(SEMNrm)[length(colnames(SEMNrm))])
      SEMNrmMLT$id <- rownames(SEMNrmMLT)
      colnames(SEMNrmMLT)[2:3] <- c("variableSEM", "plotErr") # give unique variable names. same below
      DfPlt<-merge(MeanNrmMLT,SEMNrmMLT,by = c("id","Condition"),sort=FALSE)
    } else if (errorbar == "SD"){
      SDNrmMLT <- melt(SDNrm, id.vars = colnames(SDNrm)[length(colnames(SDNrm))])
      SDNrmMLT$id <- rownames(SDNrmMLT)
      colnames(SDNrmMLT)[2:3] <- c("variableSD", "plotErr")
      DfPlt <- merge(MeanNrmMLT,SDNrmMLT,by = c("id","Condition"),sort=FALSE)
    } else {stop("Please properly specify the error bar type, SEM or SD")}
  }

  if (Nrm == FALSE) {
    ## calculate mean and SEM
    Mean <- sapply(colnames(rawData)[-1],
                 function(i)tapply(rawData[[i]], rawData[1], mean, na.rm = TRUE))
    Mean <- data.frame(Mean)
    Mean$Condition <- factor(rownames(Mean), levels = c(rownames(Mean)))

    if (errorbar == "SEM"){
      SEM <- sapply(colnames(rawData)[-1],
                  function(i)tapply(rawData[[i]], rawData[1],
                                     function(j)sd(j,na.rm = TRUE) / sqrt(length(!is.na(j)))))
      SEM <- data.frame(SEM)
      SEM$Condition<-factor(rownames(SEM),levels = c(rownames(SEM)))
    } else if (errorbar == "SD"){
      SD <- sapply(colnames(rawData)[-1],
                  function(i) tapply(rawData[[i]], rawData[1],
                                     function(j)sd(j,na.rm = TRUE)))
      SD <- data.frame(SD)
      SD$Condition <- factor(rownames(SD),levels = c(rownames(SD)))
    } else {stop("Please properly specify the error bar type, SEM or SD")}


    ## generate the master dataframe
    MeanMLT <- melt(Mean,id.vars = colnames(Mean)[length(colnames(Mean))])
    MeanMLT$id <- rownames(MeanMLT)
    colnames(MeanMLT)[3] <- "plotMean"

    if (errorbar == "SEM"){
      SEMMLT <- melt(SEM,id.vars = colnames(SEM)[length(colnames(SEM))])
      SEMMLT$id <- rownames(SEMMLT)
      colnames(SEMMLT)[2:3] <- c("variableSEM","plotErr")
      DfPlt <- merge(MeanMLT,SEMMLT,by = c("id", "Condition"), sort = FALSE)
    } else if (errorbar == "SD"){
      SDMLT <- melt(SD,id.vars = colnames(SD)[length(colnames(SD))])
      SDMLT$id <- rownames(SDMLT)
      colnames(SDMLT)[2:3] <- c("variableSD","plotErr")
      DfPlt <- merge(MeanMLT,SDMLT,by = c("id", "Condition"), sort = FALSE)
    } else {stop("Please properly specify the error bar type, SEM or SD")}
  }

  ## calculate optimal lower/upper limits (lw_lmt/upr_lmt) and major tick range (rd_intvl)
  # setting the raw y lower/upper limit
  ifelse(DfltZero == FALSE, Mn<-with(DfPlt, floor(min(plotMean - plotErr) / 0.5) * 0.5), Mn <- 0) # determine the raw lower limit, if not using the default 0.
  Mx <- with(DfPlt, ceiling((max(plotMean + plotErr) + 0.09) / 0.5) * 0.5) # determine the raw upper limit. Note it matches the settings with frogplots()

  Rge <- Mx - Mn # range between maximum and minimum values
  raw_intvl <- Rge / nMajorTicks # nMjoarTicks: excluding 0 (or the ymin)
  Aa <- 10^ceiling(log10(raw_intvl)) # find the n (ceiling(log10(2/4))) that will scale numbers down to between 0 and 1.
  rd_intvl <- ceiling((raw_intvl / Aa) / 0.05) * 0.05 * Aa # optimal and rounded major tick range
  lw_lmt <- rd_intvl * floor(Mn / rd_intvl) # optimal lower bound
  upr_lmt <- rd_intvl * ceiling(Mx / rd_intvl) # optimal upper bound. use ceiling() and remove the +1 from the original "1+Mx/rd_intvl"

  ## calculate minor tick options
  # set 4 as the minor tick range cutoff: it always makes sure to give at least 4 minor ticks.
  # if not, a decimal sacling factor (i=1) will be applied
  if (max(sapply(all_dvsr(rd_intvl), function(i)rd_intvl / i-1)) < 4){
    minor_tick_n <- sapply(all_dvsr(rd_intvl, 1), function(i)round(rd_intvl / i - 1)) # make sure to use round() for the stupid machine behaviors. same below
  } else {
    minor_tick_n <- sapply(all_dvsr(rd_intvl), function(i)round(rd_intvl / i - 1))
  }

  ## results
  Lst <- list(y_axis_range = c(lw_lmt, upr_lmt, rd_intvl), minor_tick_options = sort(minor_tick_n))
  names(Lst[[1]]) <- c("lower_limit", "upper_limit", "major_tick_range")

  return(Lst)
}
