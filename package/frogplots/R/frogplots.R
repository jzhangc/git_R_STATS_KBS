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
revsort<-function(x){
  uLst<-unlist(strsplit(x, "-"))
  uLst<-uLst[c(2,1)]
  uLst<-paste(uLst,collapse = "-")
  uLst
}

#' @title frogplots
#'
#' @description A simple to use function for plotting basing on the statistical analysis of choice.
#' @param fileName Input file name. Case sensitive and be sure to type with quotation marks. Currently only takes \code{.csv} files.
#' @param Tp Type of the intended statistical test. Case sensitive and be sure to type with quotation marks. Options are: "t-test", "ANOVA", "Tukey" and "Dunnett". Default is "Tukey".
#' @param xAngle The rotation angle (degrees) of the x axis marks.Default is \code{0} - horizontal.
#' @param xAlign The alignment type of the x axis marks. Options are \code{0}, \code{0.5} and \code{1}. The default value at \code{0} is expecially useful when \code{xAngle = 90}.
#' @param Title The displayed title on top of the plot. Be sure to type with quotation marks. Default is \code{NULL}.
#' @param xLabel x axis label. Type with quotation marks. Default is \code{NULL}
#' @param yLabel y axis label. Type with quotation marks. Default is \code{NULL}
#' @param legendTtl Hide/Display legend title. If \code{TRUE} or \code{T}, the name of the first column of the raw date file will display as the legend title. Default is \code{FALSE}.
#' @param plotWidth The width of the plot (unit: mm). Default is 170. Default will fit most of the cases.
#' @param plotHeight The height of the plot (unit: mm). Default is 150. Default will fit most of the cases.
#' @return Outputs a \code{.csv} file with detailed metrics for the plot, including Mean, SEM and significance labels, as well as a plot image file (\code{.pdf}), with 600 dpi resolution.
#' @importFrom reshape2 melt
#' @importFrom multcompView multcompLetters
#' @importFrom multcomp glht mcp
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gtable gtable_add_cols gtable_add_grob
#' @import ggplot2
#' @examples
#' \dontrun{
#' frogplots("data.csv",Tp="Tukey",yLabel="Relative fluorescence level")
#' frogplots("data2.csv",Tp="t-test",xAngle = -90, xAlign=0,yLabel="Relative fluorescence level")
#' frogplots("data3.csv",Tp="Tukey",yLabel="Relative fluorescence level")
#' frogplots("data4.csv",Tp="Dunnett", yLabel="Relative fluorescence level")
#' frogplots("data5.csv",Tp="Tukey",yLabel="Relative fluorescence level", plotWidth = 300)
#' }
#' @export
frogplots<-function(fileName, Tp="Tukey", xAngle=0, xAlign=0.5, Title=NULL, xLabel=NULL, yLabel=NULL,
                   legendTtl=FALSE, plotWidth = 170, plotHeight = 150){

  ## load file
  rawData<-read.csv(file=fileName,header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]],levels=c(unique(rawData[[1]])))

  ## normalize everything to control as 1
  Mean<-sapply(colnames(rawData)[-1],
               function(i) tapply(rawData[[i]], rawData[1], mean, na.rm=TRUE))
  Mean<-data.frame(Mean)
  Mean$Condition<-factor(rownames(Mean),levels=c(rownames(Mean)))
  MeanNrm<-data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                             function(i)sapply(Mean[[i]],function(j)j/Mean[[i]][1])),
                      Condition = factor(rownames(Mean),levels=c(rownames(Mean))))

  SEM<-sapply(colnames(rawData)[-1],
              function(i) tapply(rawData[[i]], rawData[1],
                                 function(j)sd(j,na.rm=TRUE)/sqrt(length(!is.na(j)))))
  SEM<-data.frame(SEM)
  SEM$Condition<-factor(rownames(SEM),levels=c(rownames(SEM)))
  SEMNrm<-data.frame(sapply(colnames(SEM)[-length(colnames(SEM))],
                            function(i)sapply(SEM[[i]],function(j)j/Mean[[i]][1])),
                     Condition = factor(rownames(SEM),levels=c(rownames(SEM))))
  colnames(SEMNrm)[-length(colnames(SEMNrm))]<-sapply(colnames(rawData)[-1],
                                                      function(x)paste(x,"SEM",sep=""))


  ## for automatic significant labels (Tukey: letters; t-test & Dunnett: asterisks)
  cNm<-colnames(rawData)

  Tt<-sapply(colnames(rawData)[-1],
             function(i) {
               fml<-paste(i,cNm[1],sep="~")
               Mdl<-aov(formula(fml),data=rawData)
               # below: make sure to chain if else in this way.
               if (Tp=="t-test"){
                 if (nlevels(rawData[[1]])==2){
                   Control<-subset(rawData[i],rawData[[1]] == levels(rawData[[1]])[1])
                   Experimental<-subset(rawData[i],rawData[[1]] == levels(rawData[[1]])[2])
                   Ttest<-t.test(Control,Experimental,na.rm=TRUE)
                   Ttestp<-Ttest$p.value
                   Lvl<- data.frame(Condition=unique(rawData[[1]]),pvalue=c(1,Ttestp))
                   Lvl$Lbl<-sapply(Lvl$pvalue,function(x)ifelse(x<0.05,"*",""))
                   Lvl<-Lvl[,c(1,3)]
                 } else {stop("T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try ANOVA/Tukey/Dunnett).")}
               } else if (Tp=="Tukey"){
                 if (nlevels(rawData[[1]])>2){
                   Sts<-TukeyHSD(Mdl)
                   Tkp<-Sts[[1]][,4]
                   names(Tkp)<-sapply(names(Tkp),function(j)revsort(j)) # change orders (from b-a to a-b)
                   Tkp<-multcompLetters(Tkp)["Letters"] # from the multcompView package.
                   Lbl<-names(Tkp[["Letters"]])
                   Lvl<-data.frame(Lbl, Tkp[["Letters"]],
                                   stringsAsFactors = FALSE)
                 } else {stop("USE T-TEST FOR A TWO-GROUP COMPARISON")}
               } else if (Tp=="Dunnett"){
                 if (nlevels(rawData[[1]])>2){
                   var <- cNm[1]
                   arg<- list("Dunnett")
                   names(arg)<-var
                   mcp<- do.call(mcp, arg)
                   Sts<-summary(glht(Mdl, linfct=mcp))
                   Dnt<-Sts$test$pvalues
                   names(Dnt)<-names(Sts$test$coefficients)
                   Lvl<- data.frame(Condition=unique(rawData[[1]]),pvalue=c(1,Dnt))
                   Lvl$Lbl<-sapply(Lvl$pvalue,function(x)ifelse(x<0.05,"*",""))
                   Lvl<-Lvl[,c(1,3)]
                 } else {stop("USE T-TEST FOR A TWO-GROUP COMPARISON")}
               } else {
                 stop("ERROR: CHECK YOUR SPELLING (Hint: EveRyThinG iS cASe-sEnSiTiVE).")
               }
               colnames(Lvl)<-c(colnames(rawData)[1],i)
               Lvl
             },simplify = FALSE)
  cTt <- Reduce(function(x, y) merge(x, y, all=T,
                                     by=colnames(rawData)[1],sort=FALSE),
                Tt, accumulate=FALSE)
  colnames(cTt)[-1]<-sapply(colnames(rawData)[-1],
                            function(x)paste(x,"Lbl",sep=""))

  ## generate the master dataframe for plotting
  MeanNrmMLT<-melt(MeanNrm,id.vars=colnames(MeanNrm)[length(colnames(MeanNrm))]) # melt mean
  MeanNrmMLT$id<-rownames(MeanNrmMLT)

  SEMNrmMLT<-melt(SEMNrm,id.vars=colnames(SEMNrm)[length(colnames(SEMNrm))]) # melt SEM
  SEMNrmMLT$id<-rownames(SEMNrmMLT)

  cTtMLT<-melt(cTt,id.vars=colnames(cTt)[1]) # melt labels
  cTtMLT$id<-rownames(cTtMLT)
  cTtMLT[1]<-as.factor(cTtMLT[[1]])

  colnames(MeanNrmMLT)[3]<- "NrmMean" # give unique variable names
  colnames(SEMNrmMLT)[2:3]<-c("variableSEM","NrmSEM") # give unique variable names
  colnames(cTtMLT)[1:3]<-c(colnames(MeanNrmMLT)[1],"variableLbl","Lbl")

  DfPlt<-merge(MeanNrmMLT,SEMNrmMLT,by=c("id","Condition"),sort=FALSE)
  DfPlt<-merge(DfPlt,cTtMLT,by=c("id","Condition"),sort=FALSE)

  # dump all data into a file
  write.csv(DfPlt,file=paste(substr(noquote(fileName),1,nchar(fileName)-4),".plot.csv",sep=""),
            quote=FALSE,na="NA",row.names = FALSE)

  ## plotting
  # a function to add minor ticks
  minor_tick <- function(major, n_minor) {
    labs <- c( sapply(major, function(x) c(x, rep("", n_minor) ) ) )
    labs[1:(length(labs)-n_minor)]
  }

  loclEnv<-environment()
  baseplt<-ggplot(data=DfPlt, aes(x=variable, y=NrmMean,fill=Condition),
                  environment = loclEnv) +
    geom_bar(position="dodge",stat="identity",color="black")+
    geom_errorbar(aes(ymin=NrmMean-NrmSEM,ymax=NrmMean+NrmSEM),width=0.2,
                  position=position_dodge(0.9))+
    scale_y_continuous(expand = c(0, 0),
                       breaks= seq(0,ceiling(with(DfPlt,max(NrmMean+NrmSEM)+0.07)/0.5)*0.5,by=0.1),
                       labels = minor_tick(seq(0, ceiling(with(DfPlt,max(NrmMean+NrmSEM)+0.07)/0.5)*0.5, by=0.5), 4),
                       limits=c(0,ceiling(with(DfPlt,max(NrmMean+NrmSEM)+0.07)/0.5)*0.5))+
    ggtitle(Title)+
    xlab(xLabel)+
    ylab(yLabel)+
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="bottom",
          axis.text.x = element_text(size=10, angle=xAngle,hjust=xAlign),
          axis.text.y = element_text(size=10, hjust=0.5))+
    scale_fill_grey(start=0)

  if (Tp=="Tukey"){
    pltLbl<-baseplt+
      geom_text(aes(y = NrmMean+NrmSEM+0.1,label = Lbl), position = position_dodge(width=0.9),
                color="black") # the labels are placed 0.1 (tested optimal for letters) unit higher than the mean+SEM.
  } else {
    pltLbl<-baseplt+
      geom_text(aes(y = NrmMean+NrmSEM+0.06,label = Lbl), position = position_dodge(width=0.9),
                size=6, color="black") # font size 6 and 0.06 unit higher is good for asterisks.
  }

  if (legendTtl==FALSE){
    pltLbl<-pltLbl+theme(legend.title=element_blank())
  }

  if (nlevels(DfPlt$variable)==1){
    plt<-pltLbl+
      theme(axis.text.x = element_blank())+
      coord_equal(ratio=0.5)+
      scale_x_discrete(expand = c(0.1, 0.1)) # space between y axis and fist/last bar
  } else {
    plt<-pltLbl
  }

  ## add the right-side y axis
  grid.newpage()

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

  # export the file and draw a preview
  ggsave(filename=paste(substr(noquote(fileName),1,nchar(fileName)-4),".plot.pdf",sep=""),plot=pltgtb,
         width = plotWidth, height = plotHeight, units = "mm",dpi=600)
  grid.draw(pltgtb) # preview
}
