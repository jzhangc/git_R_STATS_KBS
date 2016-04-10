.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Written by Jing Zhang, Ph.D. Please direct questions to jzhangc@gmail.com.")
  return(TRUE)
}


#' @title frogstats
#'
#' @description A simple to use function for comprehensive statistical analyses.
#' @param fileName Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Tp Type of the intended statistical test. Case sensitive and type with quotation marks. Options are: "t-test", "ANOVA", "Tukey" and "Dunnett". Default is "ANOVA".
#' @return Outputs a \code{.txt} file with Shapiro-Wilk normality test results and the results of the statistical analysis of interest.
#' @importFrom multcomp glht mcp
#' @examples
#' \dontrun{
#' frogstats("data.csv","Tukey")
#' frogstats("data2.csv","t-test")
#' }
#' @export
frogstats<-function(fileName,Tp="ANOVA"){
  rawData<-read.csv(file=fileName,header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]],levels=c(unique(rawData[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "type-in" order

  cNm<-colnames(rawData)
  sink(file=paste(substr(noquote(fileName),1,nchar(fileName)-4),".stats.txt",sep=""),append=FALSE)
  # below: Shapiro-Wilk normality test. p>0.5 means the data is normal.
  print(sapply(cNm[-1],
               function(i)tapply(rawData[[i]],rawData[1],function(x)shapiro.test(x)),
               simplify = FALSE))
  # below: stats
  print(sapply(cNm[-1], function(x){
    fml<-paste(x,cNm[1],sep="~")
    Mdl<-aov(formula(fml),data = rawData) # fit an analysis of variance model by a call to lm(), applicable for both balanced or unbalanced data set.
    if (Tp=="t-test"){
      if (nlevels(rawData[[1]])==2){
        Control<-subset(rawData[x],rawData[[1]] == levels(rawData[[1]])[1])
        Experimental<-subset(rawData[x],rawData[[1]] == levels(rawData[[1]])[2])
        Eqv<-bartlett.test(formula(fml),data = rawData) # Bartlett equal variance test. p>0.5 means the variance between groups are unequal.
        tTest<-t.test(Control, Experimental, var.equal = TRUE ,na.rm = TRUE)
        statsLst<-list(EqualVariance = Eqv, ttest = tTest)
      } else {"T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try ANOVA/Tukey/Dunnett)."}
    } else if (Tp =="ANOVA"){
      if (nlevels(rawData[[1]])>2){
        Eqv<-bartlett.test(formula(fml),data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups are unequal.
        statsLst<-list(EqualVariance = Eqv, ANOVA = anova(Mdl))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else if (Tp =="Tukey"){
      if (nlevels(rawData[[1]])>2){
        Eqv<-bartlett.test(formula(fml),data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups are unequal.
        statsLst<-list(EqualVariance = Eqv, ANOVA = anova(Mdl), Tukey = TukeyHSD(Mdl))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else if (Tp =="Dunnett"){
      if (nlevels(rawData[[1]])>2){
        Eqv<-bartlett.test(formula(fml),data = rawData)  # Bartlett equal variance test. p>0.5 means the variance between groups are unequal.
        var<-cNm[1]
        arg<-list("Dunnett")
        names(arg)<-var
        mcp<-do.call(mcp, arg)
        statsLst<-list(EqualVariance = Eqv, ANOVA = anova(Mdl), Dunnett = summary(glht(Mdl, linfct = mcp)))
        statsLst
      } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
    } else {
      "ERROR: CHECK YOUR SPELLING (Hint: EveRyThinG iS cASe-sEnSiTiVE)."
    }
  },  simplify = FALSE)
  )
  sink() # end the dump
}
