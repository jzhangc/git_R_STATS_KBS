rm(list=ls(all=TRUE))
setwd("~/OneDrive/Storey lab/current_work/switch_to_R/sample_data/")

###### Statistical analysis
## next step: use with()
## install and load the pacakge: multcomp (for Dunnett)
require(multcomp) # this package is for Dunnett test

#### stats
## function time
allStats<-function(fileName,Tp="ANOVA"){
  rawData<-read.csv(file=fileName,header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]],levels=c(unique(rawData[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "type-in" order
  
  cNm<-colnames(rawData)
  require(multcomp) # found a way to take this line out of the function
  
  # below: nchar() counts the number of the characters: note the diference between length(), 
  # which counts "how many" the *whole* character strings.
  # ALSO, to use substr(), the object has to have "no quote" - use the function noquote() to achieve.
  sink(file=paste(substr(noquote(fileName),1,nchar(fileName)-4),".stats.txt",sep=""),append=TRUE) # start the dump. 
  # below: Shapiro-Wilk normality test. p>0.5 means the data is normal.
  print(sapply(cNm[-1],
               function(i)tapply(rawData[[i]],rawData[1],function(x)shapiro.test(x)),
               simplify = FALSE))
  # below: stats
  print(sapply(cNm[-1], function(x){
    fml<-paste(x,cNm[1],sep="~")
    Mdl<-aov(formula(fml),data=rawData)
    # below: make sure to chain if else in this way!
    if (Tp=="t-test"){
      if (nlevels(rawData[[1]])==2){
        Control<-subset(rawData[x],rawData[[1]] == levels(rawData[[1]])[1])
        Experimental<-subset(rawData[x],rawData[[1]] == levels(rawData[[1]])[2])
        t.test(Control,Experimental,na.rm=TRUE)
      } else {"T-TEST CAN ONLY BE DONE FOR A TWO-GROUP COMPARISON (hint: try ANOVA/Tukey/Dunnett)."}
    } else if (Tp=="ANOVA"){
      if (nlevels(rawData[[1]])>2){
        anova(Mdl)
        } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
      } else if (Tp=="Tukey"){
        if (nlevels(rawData[[1]])>2){
          TukeyHSD(Mdl)
        } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
      } else if (Tp=="Dunnett"){
        if (nlevels(rawData[[1]])>2){
          var <- cNm[1]
          arg<- list("Dunnett")
          names(arg)<-var
          mcp<- do.call(mcp, arg)
          summary(glht(Mdl, linfct=mcp)) 
        } else {"USE T-TEST FOR A TWO-GROUP COMPARISON"}
      } else {
          "ERROR: CHECK YOUR SPELLING (Hint: EveRyThinG iS cASe-sEnSiTiVE)."
            } # but the function will still dump the result of the normality test.
  },  simplify = FALSE)
  )
  sink() # end the dump
}

allStats("data.csv","Tukey")
allStats("data2.csv","t-test")


#### plotting
allPlots<-function(fileName, Tp="Tukey", Title=NULL,xAngle=0, xAlign=0.5,xLabel=NULL,yLabel=NULL,
                   plotWidth = 170, plotHeight = 150){
  require(ggplot2)
  require(reshape2)
  require(multcompView) # for Tukey significant labels
  require(multcomp) # for Dunnett test
  require(gtable) # for duplicating y axis on the right side
  require(grid) # for duplicating y axis on the right side 
  
  ## load file
  rawData<-read.csv(file=fileName,header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
  rawData[[1]]<-factor(rawData[[1]],levels=c(unique(rawData[[1]]))) # avoid R's automatic re-ordering the factors automatically - it will keep the "type-in" order
  
  ## normalize everything to control as 1
  Mean<-sapply(colnames(rawData)[-1], 
                  function(i) tapply(rawData[[i]], rawData[1], mean, na.rm=TRUE))
  Mean<-data.frame(Mean)
  Mean$Condition<-factor(rownames(Mean),levels=c(rownames(Mean)))
  MeanNrm<-data.frame(sapply(colnames(Mean)[-length(colnames(Mean))],
                                function(i)sapply(Mean[[i]],function(j)j/Mean[[i]][1])),
                         Condition = factor(rownames(Mean),levels=c(rownames(Mean)))) # to keep the correct factor level order.

  SEM<-sapply(colnames(rawData)[-1], 
                 function(i) tapply(rawData[[i]], rawData[1], 
                                    function(j)sd(j,na.rm=TRUE)/sqrt(length(!is.na(j)))))
  SEM<-data.frame(SEM)
  SEM$Condition<-factor(rownames(SEM),levels=c(rownames(SEM)))
  SEMNrm<-data.frame(sapply(colnames(SEM)[-length(colnames(SEM))],
                               function(i)sapply(SEM[[i]],function(j)j/Mean[[i]][1])),
                        Condition = factor(rownames(SEM),levels=c(rownames(SEM)))) # to keep the correct factor level order.
  colnames(SEMNrm)[-length(colnames(SEMNrm))]<-sapply(colnames(rawData)[-1],
                                  function(x)paste(x,"SEM",sep=""))

  
  ## for automatic significant labels (Tukey: letters; t-test & Dunnett: asterisks)
  
  # below: a function swtiches the order around the "-" symbol of a character string: from "a-b" to "b-a".
  revSort<-function(x){
    uLst<-unlist(strsplit(x, "-"))
    uLst<-uLst[c(2,1)]
    uLst<-paste(uLst,collapse = "-") # collapse pastes things and puts them into ONE string object.
    uLst
  }
  
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
                   names(Tkp)<-sapply(names(Tkp),function(j)revSort(j)) # change orders (from b-a to a-b)
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
                Tt, accumulate=FALSE) # Reduce() higher level funtion to contain other fucntions in functional programming 
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
  colnames(cTtMLT)[2:3]<-c("variableLbl","Lbl") # same as above
  
  DfPlt<-merge(MeanNrmMLT,SEMNrmMLT,by=c("id","Condition"),sort=FALSE)
  DfPlt<-merge(DfPlt,cTtMLT,by=c("id","Condition"),sort=FALSE)
  
  # dump all data into a file
  write.csv(DfPlt,file=paste(substr(noquote(fileName),1,nchar(fileName)-4),".plot.csv",sep=""),
            quote=FALSE,na="NA",row.names = FALSE)
  
  ## plotting
  # below: a function that allows to insert minor ticks
  insert_minor <- function(major_labs, n_minor) {
    labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
    labs[1:(length(labs)-n_minor)]
  }
  
  loclEnv<-environment()
  baseplt<-ggplot(data=DfPlt, aes(x=variable, y=NrmMean,fill=as.factor(Condition)),
              environment = loclEnv) +
    geom_bar(position="dodge",stat="identity",color="black")+
    geom_errorbar(aes(ymin=NrmMean-NrmSEM,ymax=NrmMean+NrmSEM),width=0.2,
                  position=position_dodge(0.9))+
    scale_y_continuous(expand = c(0, 0),
                       breaks= seq(0,ceiling(with(DfPlt,max(NrmMean+NrmSEM))/0.5)*0.5,by=0.1),
                       labels = insert_minor(seq(0, ceiling(with(DfPlt,max(NrmMean+NrmSEM))/0.5)*0.5, by=0.5), 4),
                       limits=c(0,ceiling(with(DfPlt,max(NrmMean+NrmSEM))/0.5)*0.5))+
    ggtitle(Title)+
    xlab(xLabel)+
    ylab(yLabel)+
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          legend.position="bottom",legend.title=element_blank(),
          axis.text.x = element_text(size=10, angle=xAngle,hjust=xAlign),
          axis.text.y = element_text(size=10, hjust=0.5))+
    scale_fill_grey(start=0,end=1)
  
  if (Tp=="Tukey"){
    pltLbl<-baseplt+    
      geom_text(aes(y = NrmMean+NrmSEM+0.1,label = Lbl), position = position_dodge(width=0.9),
                color="black") # the labels are placed 0.1 (tested optimal for letters) unit higher than the mean+SEM. 
  } else {
    pltLbl<-baseplt+
      geom_text(aes(y = NrmMean+NrmSEM+0.06,label = Lbl), position = position_dodge(width=0.9),
                size=6, color="black") # font size 6 and 0.06 unit higher is good for asterisks.
  }
  
  if (nlevels(DfPlt$variable)==1){ # nlevels() outputs the number of the factor levels
    plt<-pltLbl+
      theme(axis.text.x = element_blank())+
      coord_equal(ratio=0.5)+
      scale_x_discrete(expand = c(0.1, 0.1)) # space between y axis and fist/last bar
  } else {
    plt<-pltLbl
  }
  
  # add the right-side y axis
  grid.newpage()
  # extract gtable
  g <- ggplot_gtable(ggplot_build(plt))
  
  # axis tweaks
  ia <- which(g$layout$name == "axis-l")
  ga <- g$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.08, "cm")
  pp <- c(subset(g$layout, name == "panel", select = t:r))
  g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # export the file and draw a preview
  ggsave(filename=paste(substr(noquote(fileName),1,nchar(fileName)-4),".plot.pdf",sep=""),plot=g,
         width = plotWidth, height = plotHeight, units = "mm",dpi=600)
  grid.draw(g) # preview
}

allPlots("data.csv",Tp="Tukey",yLabel="Relative fluorescence level")
allPlots("data2.csv",Tp="t-test",xAngle = -90, xAlign=0,yLabel="Relative fluorescence level")
allPlots("data3.csv",Tp="Tukey",yLabel="Relative fluorescence level")
allPlots("data4.csv",Tp="Dunnett", yLabel="Relative fluorescence level")
allPlots("data5.csv",Tp="Tukey",yLabel="Relative fluorescence level", plotWidth = 300)

#### test realm
## sample data
tstData<-read.csv(file="data.csv",header=TRUE, na.strings = "NA", stringsAsFactors = FALSE)
tstData[[1]]<-factor(tstData[[1]],levels=c(unique(tstData[[1]])))

tstData2<-read.csv(file="data2.csv",header=TRUE, na.strings = "NA",stringsAsFactors = FALSE)
tstData2[[1]]<-factor(tstData2[[1]],levels=c(unique(tstData2[[1]])))

tstData3<-read.csv(file="data3.csv",header=TRUE, na.strings = "NA")
tstData4<-read.csv(file="data4.csv",header=TRUE, na.strings = "NA")





## test
cNm<-colnames(tstData)
fml<-paste(colnames(tstData)[2],colnames(tstData)[1],sep="~") # tstData1, protein 1
tstAovP1<-aov(formula(fml), data=tstData)
tstAnvaP1<-anova(tstAovP1)
tstTukeyP1<-TukeyHSD(tstAovP1)

## test plotting
require(ggplot2)
require(reshape2)
require(multcompView) # for getting the "a", "b", "ab" etc labels 

## normalize everything to control as 1
tstMean<-sapply(colnames(tstData)[-1], 
               function(i) tapply(tstData[[i]], tstData[1], mean, na.rm=TRUE))
tstMean<-data.frame(tstMean)
tstMean$Condition<-factor(rownames(tstMean),levels=c(rownames(tstMean)))
length(colnames(tstMean))
tstMeanNrm<-data.frame(sapply(colnames(tstMean)[-length(colnames(tstMean))],
                   function(i)sapply(tstMean[[i]],function(j)j/tstMean[[i]][1])),
                   Condition = factor(rownames(tstMean),levels=c(rownames(tstMean))))

tstSEM<-sapply(colnames(tstData)[-1], 
              function(i) tapply(tstData[[i]], tstData[1], 
                                 function(j)sd(j,na.rm=TRUE)/sqrt(length(!is.na(j)))))
tstSEM<-data.frame(tstSEM)
tstSEM$Condition<-factor(rownames(tstSEM),levels=c(rownames(tstSEM)))
tstSEMNrm<-data.frame(sapply(colnames(tstSEM)[-length(colnames(tstSEM))],
                              function(i)sapply(tstSEM[[i]],function(j)j/tstMean[[i]][1])),
                       Condition = factor(rownames(tstSEM),levels=c(rownames(tstSEM))))
colnames(tstSEMNrm)[-length(colnames(tstSEM))]<-sapply(colnames(tstData)[-1],
                                 function(x)paste(x,"SEM",sep=""))

## for getting the "a", "b", "ab" etc labels
library(multcompView)

# a function swtiches the order around the "-" symbol of a character string: from "a-b" to "b-a".
revSort<-function(x){
  uLst<-unlist(strsplit(x, "-"))
  uLst<-uLst[c(2,1)]
  uLst<-paste(uLst,collapse = "-") # collapse paste things and put them into ONE string object
  uLst
}

# set the letters
# for Tukey test
Tt<-sapply(colnames(tstData)[-1], 
           function(i) {
             fml<-paste(i,cNm[1],sep="~")
             Mdl<-aov(formula(fml),data=tstData)
             Tk<-TukeyHSD(Mdl)
             Tkp<-Tk[[1]][,4]
             names(Tkp)<-sapply(names(Tkp),function(j)revSort(j)) # change orders (from b-a to a-b)
             Tkp<-multcompLetters(Tkp)["Letters"] # from the multcompView package.
             Lbl<-names(Tkp[["Letters"]])
             Lvl<-data.frame(Lbl, Tkp[["Letters"]],
                             stringsAsFactors = FALSE)
             colnames(Lvl)<-c(colnames(tstData)[1],i)
             Lvl
           },simplify = FALSE)
cTt <- Reduce(function(x, y) merge(x, y, all=T, 
                                   by=colnames(tstData)[1],sort=FALSE), 
              Tt, accumulate=F)
colnames(cTt)[-1]<-sapply(colnames(tstData)[-1],
                          function(x)paste(x,"Lbl",sep=""))

# for dunnett test
Tkp<-tstDnt$test$pvalues
names(Tkp)<-names(tstDnt$test$coefficients)
TkpDf<- data.frame(Condition=levels(tstData[[1]]),pvalue=c(1,Tkp))
TkpDf$Lbl<-sapply(TkpDf$pvalue,function(x)ifelse(x<0.05,"*",""))
TkpDf<-TkpDf[,c(1,3)]
colnames(TkpDf)<-c(colnames(tstData)[1],colnames(tstData[2]))

Tt<-sapply(colnames(tstData)[-1], 
              function(i) {
                fDnt<-function(data){
                  require(multcomp)
                  fml<-paste(i,cNm[1],sep="~")
                  var <- colnames(data)[1]
                  arg<- list("Dunnett")
                  names(arg)<-var
                  mcp<- do.call(mcp, arg)
                  Aov<-aov(formula(fml), data=tstData)
                  Dnt<-summary(glht(Aov, linfct=mcp))
                  Dnt
                }
                tstDnt<-fDnt(tstData)
                Tkp<-tstDnt$test$pvalues
                names(Tkp)<-names(tstDnt$test$coefficients)
                TkpDf<- data.frame(Condition=unique(tstData[[1]]),pvalue=c(1,Tkp))
                TkpDf$Lbl<-sapply(TkpDf$pvalue,function(x)ifelse(x<0.05,"*",""))
                TkpDf<-TkpDf[,c(1,3)]
                colnames(TkpDf)<-c(colnames(tstData)[1],i)
                TkpDf
              },simplify = FALSE)
cTt <- Reduce(function(x, y) merge(x, y, all=T, 
                                      by=colnames(tstData)[1],sort=FALSE), 
                 Tt, accumulate=F)


# for t-test
Tt<-sapply(colnames(tstData2)[-1], 
           function(i) {
             Control<-subset(tstData2[i],tstData2[[1]] == levels(tstData2[[1]])[1])
             Experimental<-subset(tstData2[i],tstData2[[1]] == levels(tstData2[[1]])[2])
             Tk<-t.test(Control,Experimental,na.rm=TRUE)
             Tkp<-Tk$p.value
             TkpDf<- data.frame(Condition=unique(tstData2[[1]]),pvalue=c(1,Tkp))
             TkpDf$Lbl<-sapply(TkpDf$pvalue,function(x)ifelse(x<0.05,"*",""))
             TkpDf<-TkpDf[,c(1,3)]
             colnames(TkpDf)<-c(colnames(tstData2)[1],i)
             TkpDf
           },simplify = FALSE)

cTt <- Reduce(function(x, y) merge(x, y, all=T, 
                                   by=colnames(tstData2)[1],sort=FALSE), 
              Tt, accumulate=F)



## generate the master dataframe for plotting
library(reshape2)
tstMeanNrmMLT<-melt(tstMeanNrm,id.vars=colnames(tstMeanNrm)[length(colnames(tstMeanNrm))]) # melt mean
tstMeanNrmMLT$ID<-rownames(tstMeanNrmMLT)

tstSEMNrmMLT<-melt(tstSEMNrm,id.vars=colnames(tstSEMNrm)[length(colnames(tstSEMNrm))]) # melt SEM
tstSEMNrmMLT$ID<-rownames(tstSEMNrmMLT)

cTtMLT<-melt(cTt,id.vars=colnames(cTt)[1]) # melt labels
cTtMLT$ID<-rownames(cTtMLT)
cTtMLT[1]<-as.factor(cTtMLT[[1]])

colnames(tstMeanNrmMLT)[3]<- "MEAN" # give unique variable names
colnames(tstSEMNrmMLT)[2:3]<-c("variableSEM","SEM") # give unique variable names
colnames(cTtMLT)[2:3]<-c("variableLbl","Lbl") # same as above

tstDfPlt<-merge(tstMeanNrmMLT,tstSEMNrmMLT,by=c("ID","Condition"),sort=FALSE)
tstDfPlt<-merge(tstDfPlt,cTtMLT,by=c("ID","Condition"),sort=FALSE)

str(tstDfPlt)

## ploting
# a function that allows to insert minor ticks
insert_minor <- function(major_labs, n_minor) {
  labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  labs[1:(length(labs)-n_minor)]
}


plt<-ggplot(tstDfPlt, aes(x=variable, y=MEAN,fill=as.factor(Condition))) +
  geom_bar(position="dodge",stat="identity",color="black")+
  geom_errorbar(aes(ymin=MEAN-SEM,ymax=MEAN+SEM),width=.2,
                position=position_dodge(.9))+
  scale_y_continuous(expand = c(0, 0),
                     breaks= seq(0,ceiling(with(tstDfPlt,max(MEAN+SEM))/0.5)*0.5,by=0.1),
                     # below: ceiling(with(tstDfPlt,max(MEAN+SEM))/0.5)*0.5 is to always round up to the nearest 0.5 (let it be "b"), 
                     # following the equation: ceiling(a/b)*b
                     labels = insert_minor(seq(0, ceiling(with(tstDfPlt,max(MEAN+SEM))/0.5)*0.5, by=0.5), 4),
                     limits=c(0,with(tstDfPlt,ceiling(with(tstDfPlt,max(MEAN+SEM))/0.5)*0.5)))+
  ggtitle(NULL)+
  xlab(NULL)+ # we can hide it using NULL
  ylab("test")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        legend.position="bottom",legend.title=element_blank(),
        axis.text.x = element_text(size=10, angle=90,hjust=1),
        axis.text.y = element_text(size=10, hjust=0.5))+
  scale_fill_grey(start=0,end=1)+
  coord_equal(ratio=1.5)+
  scale_x_discrete(expand = c(0.08, 0.08))+
  geom_text(aes(y = MEAN+SEM+0.05,label = Lbl),position = position_dodge(width=0.9),size=6,
            color="black")

if (nlevels(tstDfPlt$variable)==1){ # nlevels() outputs the number of the factor levels
  plt+theme(axis.text.x = element_blank())
} else {
  plt 
}

## below: duplicate the y axis on the right side
library(gtable)
library(grid)
grid.newpage()
# extract gtable
g <- ggplot_gtable(ggplot_build(plt))

# axis tweaks
ia <- which(g$layout$name == "axis-l")
ga <- g$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.08, "cm")
pp <- c(subset(g$layout, name == "panel", select = t:r))
g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
ggsave(file="test.pdf", g)

# draw it
grid.draw(g)




## the function below allows to insert minor ticks
insert_minor <- function(major_labs, n_minor) {
  labs <- c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
  labs[1:(length(labs)-n_minor)]
}

## to be used
tstLst<-list(
  tstCond=tstData[[1]],
  tstRaw=tstData,
  tstN<-sapply(colnames(tstData)[-1], 
               function(i) tapply(tstData[[i]], tstData$Condition, 
                                  function(j)length(j[!is.na(j)]))), # the n value, NAs won't count
  tstMean=sapply(colnames(tstData)[-1], 
                 function(i) tapply(tstData[[i]], tstData$Condition, mean, na.rm=TRUE)),
  tstSD=sapply(colnames(tstData)[-1], 
               function(i) tapply(tstData[[i]], tstData$Condition, sd, na.rm=TRUE)),
  tstSEM=sapply(colnames(tstData)[-1], 
                function(i) tapply(tstData[[i]], tstData$Condition, 
                                   function(j)sd(j,na.rm=TRUE)/sqrt(length(!is.na(j))))),
  tstNrm=sapply(colnames(tstData)[-1],
                function(i)tapply(tstData[[i]],tstData$Condition,function(x)shapiro.test(x)),
                simplify = FALSE)
)



#### develop
## ANOVA
tstNm<-colnames(tstData) # get rid of the first variable "condition"
sink(file="Out.txt",append=TRUE)
print(sapply(tstNm[-1],function(x){
  fml<-paste(x,tstNm[1],sep="~")
  Mdl<-aov(formula(fml),data=tstData)
  anova(Mdl)
}, simplify = FALSE))
sink()

## TUKEY
tstNm<-colnames(tstData) # get rid of the first variable "condition"
sink(file="Out.txt",append=TRUE)
sapply(tstNm[-1],function(x){
  fml<-paste(x,tstNm[1],sep="~")
  Mdl<-aov(formula(fml),data=tstData)
  anova(Mdl)
  TukeyHSD(Mdl)
}, simplify = FALSE)
sink()

## DUNNETT
#The tricky bit is that mcp() interprets its first argument in a nonstandard way. Within mcp(), 
#x1 = 'Dunnett' does not (as it normally would) mean "assign a value of 'Dunnett' to the argument x1". 
#Instead, the whole thing is interpreted as a symbolic description of the intended contrasts. 
#(In this, it is much like more familiar formula objects such as the y ~ x1 + x2 in the fitting call).

require(multcomp)
tstNm<-colnames(tstData) # get rid of the first variable "condition"
sink(file="Out.txt",append=TRUE)
sapply(tstNm[-1],function(x){
  fml<-paste(x,tstNm[1],sep="~")
  Mdl<-aov(formula(fml),data=tstData)
  ## below: the shitty multcomp piece of fu*king sh*t mcp()
  var <- tstNm[1]
  arg<- list("Dunnett")
  names(arg)<-var
  mcp<- do.call(mcp, arg)
  summary(glht(Mdl, linfct=mcp))
}, simplify = FALSE)
sink()

rm(mcp)
glht(fm1, linfct = cmp)

# below: a function for one target Dunnett test
fDnt<-function(data){
  require(multcomp)
  var <- colnames(data)[1]
  arg<- list("Dunnett")
  names(arg)<-var
  mcp<- do.call(mcp, arg)
  Aov<-aov(formula(fml), data=tstData)
  Dnt<-summary(glht(Aov, linfct=mcp))
  Dnt
}
tstDnt<-fDnt(tstData)




#### extra
fA<-function(x){
  fB<-function(Output,n){
    if (n>length(colnames(x))-1){Output}
    else{
      fB(tapply(tstData[,n+1], tstData$Condition, mean, na.rm=TRUE),n+1)
    }
  }
  fB(matrix(),1)
}

Rnm<-colnames(tstData[,-1])
Aa<-tapply(tstData[,2], tstData$Condition, mean)

sink(file="testOut.txt", append=TRUE) # start the dump
tstTukey1
"-------------------------"
""
sink(file="testOut.txt", append=TRUE)
tstTukey2
"-------------------------"
""
sink() # end of the dump






fml<-paste(colnames(tstData)[2],colnames(tstData)[1],sep="~")
tstAov<-aov(formula(fml), data=tstData)
anova(tstAov)
TukeyHSD(tstAov)
var <- colnames(tstData)[1]
arg<- list("Dunnett")
names(arg)<-var
mcp<- do.call(mcp, arg)
summary(glht(tstAov, linfct=mcp))# dunnett test, mcp() specifies the control?

