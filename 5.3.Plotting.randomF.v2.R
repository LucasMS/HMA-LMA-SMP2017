library(gplots)
library(plyr)
library(SDMTools)
library(reshape)
library("RColorBrewer")
brewer.pal(3,"Dark2")


###The results seems to be all the same, I will go on with just one analysis.

#### Add weights

meta=read.csv("Data/01metadata.Train.csv", stringsAsFactors = F)
meta=meta[,c("Species", "MicAbundance")]
meta=unique(meta)
meta.c=count(meta, "MicAbundance")
wt=(meta.c$freq)

#
meta=merge.data.frame(meta, meta.c, by="MicAbundance")
meta$freq=1/meta$freq



i=1
dato=paste('4.1.random.forest/R4.1.all.', '.out', sep=as.character(i))


##################

dato=read.delim(dato, header=F, stringsAsFactors = F)
dato=dato[,c(1,3,5,6)]
colnames(dato)=c("Species", "Correctly.classified", "Level", "Number.of.estimators" )

###
dato=dato[dato$Level %in% c("01phylum",  "02class"   ),]
###
dim(dato)
###
dato=merge.data.frame(dato, meta, by="Species")
###
dim(dato)

dato$Level=paste(dato$Number.of.estimators,dato$Level, sep="!" )




dato.new=data.frame(Level=character(),
                    Mean=numeric(), 
                    Standart.deviation=numeric(),
                    stringsAsFactors=FALSE) 
n=0
for (z in unique(dato$Level)){
  n=n+1
  dato.temp=dato[dato$Level == z,]
  dato.new[n,"Level"]=z
  dato.new[n, "Mean"]=wt.mean(dato.temp$Correctly.classified, dato.temp$freq)
  dato.new[n, "Standart.deviation"]=wt.sd(dato.temp$Correctly.classified, dato.temp$freq)

}

new.cols=colsplit(dato.new$Level, "!" , c("Number.of.estimators", "Level"))

dato.new$Level=new.cols$Level
dato.new$Number.of.estimators=new.cols$Number.of.estimators
dato.new$Level=as.character(dato.new$Level)


###Get averages and Sd for the iterations
av=aggregate(dato.new$Mean, by=list(dato.new$Number.of.estimators), mean)
sd=aggregate(dato.new$Mean, by=list(dato.new$Number.of.estimators), sd)

av$x= paste(round(av$x, 1), " (", round(sd$x,1), ")", sep="")



##Check limits
borders.u=dato.new$Mean + dato.new$Standart.deviation
borders.l=dato.new$Mean - dato.new$Standart.deviation

#############









pdf("R5.randomF.pdf")
d.mean=cast(dato.new[, c("Mean", "Level", "Number.of.estimators")],  Level ~ Number.of.estimators, value="Mean")

rownames(d.mean)=d.mean[,1]
d.mean=d.mean[,-1]

d.sd=cast(dato.new[, c("Standart.deviation", "Level", "Number.of.estimators")],  Level ~ Number.of.estimators, value="Standart.deviation")



rownames(d.sd)=d.sd[,1]
d.sd=d.sd[,-1]


d.sd.u=d.mean+ d.sd
d.sd.l=d.mean- d.sd



#change names

rownames(d.mean)[rownames(d.mean)=="01phylum"]="Phylum"

rownames(d.mean)[rownames(d.mean)=="02class"]="Class"

par(mar=c(2,2,2,5))
mp=barplot2(as.matrix(d.mean), beside = TRUE,ylim = c(0, 100),
         col=brewer.pal(3,"Set3")[1:2],
         plot.ci = TRUE, ci.l = as.matrix(d.sd.l), ci.u = as.matrix(d.sd.u), legend = rownames(d.mean), names.arg =  colnames(d.mean), las=1 )

mtext(side = 3, at = colMeans(mp), line=1.3,
      text =round(colMeans(d.mean),2), cex=1)         
#legend(x=11,y=11,legend=rownames(d.mean),col=brewer.pal(3,"Dark2")[1:2],pch=1,bty="n",xpd=N)

dev.off()
