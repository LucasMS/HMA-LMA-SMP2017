library(ggplot2)
library(plyr)
library(SDMTools)
library(reshape)
library(gplots)
library(RColorBrewer)


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

pdf("R3.plot.pdf")
par(mar=c(3,3,4,8))
mp=barplot2(as.matrix(d.mean), beside = TRUE,ylim = c(0, 100),
            col=cols,
            plot.ci = TRUE, ci.l = as.matrix(d.sd.l), ci.u = as.matrix(d.sd.u), legend = rownames(d.mean), names.arg =  colnames(d.mean), las=1 )

mtext(side = 3, at = colMeans(mp), line=3,
      text =round(colMeans(d.mean),2), cex=1)         

dev.off()

#



###Overalll

tabela = cbind((aggregate(df$Mean, by=list(df$Classifier),mean)), aggregate(df$Mean, by=list(df$Classifier),sd))
tabela.names=tabela[,1]
tabela=tabela[,c(2,4)]
colnames(tabela)=c("Total (av ","& sd)")
rownames(tabela)=tabela.names
tabela=round(tabela,2)


for (i in sort(unique(df$Level))){
  df.temp=df[df$Level == i,]
  rownames(df.temp)=df.temp[, 1]
  df.temp=df.temp[, 2:3]
  colnames(df.temp)=c(paste(i, "(av"), "& sd)")
  tabela=cbind(tabela, df.temp)
}

tabela=tabela[, c(3:8,1:2)]




write.table(tabela, "R3.performance.tsv", quote = F, sep="\t")
