#' @title int_dvsr
#'
#' @description A simple to use function to find all divisors for an integer number
#' @param x An integer number
#' @return A vector containing all the divisors for the input integer
#' @examples
#' \dontrun{
#' int_dvsr(10)
#' }
#' @export
nat_dvsr<-function(x){
tstSeq<-seq_len(abs(x))
tstSeq[x %% tstSeq == 0]
}

#' @title all_dvsr
#'
#' @description A function to find all divisors for any types of number
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
all_dvsr<-function(x, i = 0){
  dopt<-getOption("scipen")
  on.exit(options(scipen = dopt))

  options(scipen = 999)
  if (x %% 1 == 0){
    smpl<-x * 10^i
    return(nat_dvsr(smpl) / 10^i)
  } else {
    n<-nchar(unlist(strsplit(as.character(x), "[.]"))[2])
    sc<-as.integer(round(x* 10^(n + i)))
    sc<-abs(sc)
    return(nat_dvsr(sc)/(10^(n + i)))
  }
}

#' @title autorange_bar_y
#'
#' @description A function to get custom lower/upper limit, major tick range, as well as minor tick options for y axis, based on a user-defined major tick number.
#' @param fileName Input file name. Data should be arranged same as the input file for \code{\link{frogplots}}.Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Nrm When \code{TRUE}, normalize data to control/first group (as 1). Default is \code{TRUE}.
#' @param nMajorTicks Number of major ticks intended to use for the plot. Note that the input number should be major tick number EXCLUDING 0 (or y axis lower limit if not using 0). Default is \code{5}.
#' @param DfltZero When \code{TRUE}, start y axis from \code{0}. Default is \code{TRUE}.
#' @importFrom reshape2 melt
#' @return A list object containing \code{lower_limit}, \code{upper_limit}, \code{major_tick_range} and \code{minor_tick_options}.
#' @examples
#' \dontrun{
#' autorange_bar_y("data.csv",Nrm = T, nMajorTicks = 8, DfltZero=FALSE)
#' }
#' @export
autorange_bar_y<-function(fileName, Nrm = TRUE, nMajorTicks = 5, DfltZero = TRUE){

  ## load file
  rawData<-read.csv(file = fileName, header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]], levels = c(unique(rawData[[1]])))

  if (Nrm == TRUE){
    ## normalize everything to control as 1
    Mean<-sapply(colnames(rawData)[-1],
                 function(i) tapply(rawData[[i]], rawData[1], mean, na.rm = TRUE))
    Mean<-data.frame(Mean)
    Mean$Condition<-factor(rownames(Mean),levels = c(rownames(Mean)))
    MeanNrm<-data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                               function(i)sapply(Mean[[i]], function(j)j/Mean[[i]][1])),
                        Condition = factor(rownames(Mean),levels = c(rownames(Mean))))

    SEM<-sapply(colnames(rawData)[-1],
                function(i) tapply(rawData[[i]], rawData[1],
                                   function(j)sd(j, na.rm = TRUE)/sqrt(length(!is.na(j)))))
    SEM<-data.frame(SEM)
    SEM$Condition<-factor(rownames(SEM), levels = c(rownames(SEM)))
    SEMNrm<-data.frame(sapply(colnames(SEM)[-length(colnames(SEM))],
                              function(i)sapply(SEM[[i]], function(j)j/Mean[[i]][1])),
                       Condition = factor(rownames(SEM), levels = c(rownames(SEM))))
    colnames(SEMNrm)[-length(colnames(SEMNrm))]<-sapply(colnames(rawData)[-1],
                                                        function(x)paste(x, "SEM", sep = ""))

    ## generate the master dataframe
    MeanNrmMLT<-melt(MeanNrm, id.vars = colnames(MeanNrm)[length(colnames(MeanNrm))])
    MeanNrmMLT$id<-rownames(MeanNrmMLT)

    SEMNrmMLT<-melt(SEMNrm, id.vars = colnames(SEMNrm)[length(colnames(SEMNrm))])
    SEMNrmMLT$id<-rownames(SEMNrmMLT)

    colnames(MeanNrmMLT)[3]<- "plotMean"
    colnames(SEMNrmMLT)[2:3]<-c("variableSEM", "plotSEM")

    DfPlt<-merge(MeanNrmMLT,SEMNrmMLT,by = c("id","Condition"),sort=FALSE)
  }

  if (Nrm == FALSE) {
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

    DfPlt<-merge(MeanMLT,SEMMLT,by = c("id", "Condition"), sort = FALSE)
  }

  ## calculate optimal lower/upper limits (lw_lmt/upr_lmt) and major tick range (rd_intvl)
  # setting the raw y lower/upper limit
  ifelse(DfltZero == FALSE, Mn<-with(DfPlt, floor(min(plotMean- plotSEM) / 0.5) * 0.5), Mn<-0)
  Mx<-with(DfPlt, ceiling((max(plotMean + plotSEM) + 0.09) / 0.5) * 0.5)

  Rge<-Mx - Mn
  raw_intvl<-Rge / nMajorTicks # nMjoarTicks: excluding 0 (or the ymin)
  Aa<-10^ceiling(log10(raw_intvl))
  rd_intvl<-ceiling((raw_intvl / Aa) / 0.05) * 0.05 * Aa
  lw_lmt<-rd_intvl * floor(Mn / rd_intvl)
  upr_lmt<-rd_intvl * ceiling(Mx / rd_intvl)

  ## calculate minor tick options
  # set 4 as the minor tick range cutoff: it always makes sure to give at least 4 minor ticks.
  # if not, a decimal sacling factor (i=1) will be applied
  if (max(sapply(all_dvsr(rd_intvl), function(i)rd_intvl / i-1)) < 4){
    minor_tick_n<-sapply(all_dvsr(rd_intvl, 1), function(i)round(rd_intvl/i - 1))
  } else {
    minor_tick_n<-sapply(all_dvsr(rd_intvl), function(i)round(rd_intvl/i - 1))
  }

  ## results
  Lst<-list(y_axis_range = c(lw_lmt, upr_lmt, rd_intvl), minor_tick_options = sort(minor_tick_n))
  names(Lst[[1]])<-c("lower_limit", "upper_limit", "major_tick_range")

  return(Lst)
}
