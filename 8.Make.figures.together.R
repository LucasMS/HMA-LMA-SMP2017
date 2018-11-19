library(ggplot2)
library(plyr)
library(SDMTools)
library(reshape)
library(gplots)
library(RColorBrewer)
pdf("Figure.standart.pdf")
par(mfrow=c(2,2))
# Create a simple example dataset
ncol=9
cols <- RColorBrewer:::brewer.pal(9,"Set3")  # OR c("purple","white","orange")  
rampcols <- colorRampPalette(colors = cols, space="Lab")(ncol)

dato=read.delim('R2.Classifiers.sel.out', header=F, stringsAsFactors = F) 

#dato=read.delim('R2.Classifiers.sel.hellinger.out', header=F, stringsAsFactors = F) 
dato=dato[,c(1,3,5,6)]
colnames(dato)=c("Species", "Correctly.classified", "Level", "Classifier" )
####Renaming dato

#dato$Level[dato$Level=='01OTU']='03OTU'
#dato$Level[dato$Level=='02phylum']='01phylum'
#dato$Level[dato$Level=='03class']='02class'

#######

#### Add weights

meta=read.csv("Data/01metadata.Train.csv", stringsAsFactors = F)
meta=meta[,c("Species", "MicAbundance")]
meta=unique(meta)
meta.c=count(meta, "MicAbundance")
wt=(meta.c$freq)

#
meta=merge.data.frame(meta, meta.c, by="MicAbundance")
meta$freq=1/meta$freq

#
dato=merge.data.frame(dato, meta, by="Species")

####

dato.o=dato[dato$Level == '03OTU',]
dato.p=dato[dato$Level == '01phylum',]
dato.c=dato[dato$Level == '02class',]


list_dato=list(dato.p, dato.c, dato.o)
names(list_dato)=c('01phylum', '02class', '03OTU')




#Mean and sd by classifier by data
for (i in 1:3){
  dato.i=list_dato[[i]]
  
  
  dato.new=data.frame(Classifier=character(),
                      Mean=numeric(), 
                      Standart.deviation=numeric(),
                      Level=character(), 
                      stringsAsFactors=FALSE) 
  n=0
  for (z in sort(unique(dato.i$Classifier))){
    n=n+1
    dato.new[n,"Classifier"]=z
    dato.temp=dato.i[dato.i$Classifier == z,]
    dato.new[n, "Mean"]=wt.mean(dato.temp$Correctly.classified, dato.temp$freq)
    dato.new[n, "Standart.deviation"]=wt.sd(dato.temp$Correctly.classified, dato.temp$freq)
    dato.new[n, "Level"]=unique(dato.temp$Level)
  }
  dato.i=dato.new
  list_dato[[i]]=dato.i
}

####Plot
df=rbind(list_dato[[1]],list_dato[[2]],list_dato[[3]])

d.mean=cast(df[, c("Mean", "Level", "Classifier")],  Classifier ~ Level , value="Mean")
rownames(d.mean)=d.mean[,1]
d.mean=d.mean[,-1]

d.sd=cast(df[, c("Standart.deviation", "Level", "Classifier")],  Classifier ~ Level, value="Standart.deviation")

rownames(d.sd)=d.sd[,1]
d.sd=d.sd[,-1]



d.sd.u=d.mean+ d.sd
d.sd.l=d.mean- d.sd

colnames(d.mean)[colnames(d.mean)=="01phylum"]="Phylum"

colnames(d.mean)[colnames(d.mean)=="02class"]="Class"

colnames(d.mean)[colnames(d.mean)=="03OTU"]="OTU"


par(mar=c(3,3,4,1))
mp=barplot2(as.matrix(d.mean), beside = TRUE,ylim = c(0, 100),
            col=cols,
            plot.ci = TRUE, ci.l = as.matrix(d.sd.l), ci.u = as.matrix(d.sd.u), 
            #legend = rownames(d.mean), 
            names.arg =  colnames(d.mean), las=1,  main="A", adj=0, font.main=1 )

mtext(side = 3, at = colMeans(mp), line=2,
      text =round(colMeans(d.mean),1), cex=0.7)         
legend(x=33, y=100, legend=rownames(d.mean), fill=cols, ncol=1, xpd=NA)

#####Second plot


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
plot.new()
par(mar=c(3,3,4,1))
mp=barplot2(as.matrix(d.mean), beside = TRUE,ylim = c(0, 100),
            col=brewer.pal(3,"Set3")[1:2],
            plot.ci = TRUE, ci.l = as.matrix(d.sd.l), ci.u = as.matrix(d.sd.u), 
            #legend = rownames(d.mean), 
            names.arg =  colnames(d.mean), las=1, main="B", adj=0, font.main=1 )

mtext(side = 3, at = colMeans(mp), line=1.3,
      text =round(colMeans(d.mean),1), cex=0.7)         
legend(x=33,y=100,legend=rownames(d.mean),fill=brewer.pal(3,"Dark2")[1:2],xpd=NA)
dev.off()
