library(ggplot2)
library('pheatmap')
library(stringr)

#select estimators
n=50


ncol=11
cols <-c("gold", "green3")
rampcols <- colorRampPalette(colors = cols, space="Lab")(ncol)


for (i in 1:10){
data.1=paste('4.1.random.forest//R4.1.all.', '.out', sep=as.character(i))
  

##################

data.1=read.delim(data.1, header=F, stringsAsFactors = F)
data.1=data.1[,c(1,3,5,6)]
colnames(data.1)=c("Species", "Correctly.classified", "Level", "Number.of.estimators" )



#####
data.1=data.1[data.1$Number.of.estimators == n,]
data.1=data.1[, -4]

###########


library(reshape2)

data.1=dcast(data.1, Species ~  Level, value.var =  "Correctly.classified")


#########
rownames(data.1) =data.1[,1]
data.1=data.1[,-1]
data.1=as.matrix(data.1)



if (i == 1){
  d1O=data.frame(data.1[,1])
  colnames(d1O)[i]=as.character(i)
  d1P=data.frame(data.1[,2])
  colnames(d1P)[i]=as.character(i)
  d1C=data.frame(data.1[,3])
  colnames(d1C)[i]=as.character(i)
 }  


if (i != 1){
  d1O=cbind(d1O,data.1[,1])
  colnames(d1O)[i]=as.character(i)
  d1P=cbind(d1P, data.1[,2])
  colnames(d1P)[i]=as.character(i)
  d1C=cbind(d1C,data.1[,3])
  colnames(d1C)[i]=as.character(i)
  
}
}
 


data.1=data.frame(Phylum=rowMeans(d1P), Class=rowMeans(d1C), OTU=rowMeans(d1O))






####Change names to include HMA and LMA labels

names=read.csv("Data/01metadata.Train.csv", stringsAsFactors = F)
names=names[,c("Species", "MicAbundance")]
names=unique(names)
names=names[  match(rownames(data.1), names$Species),  ]

identical(names$Species, rownames(data.1))


names$names=paste(names$MicAbundance, names$Species)

rownames(data.1)=names$names


data.1=data.1[order(rownames(data.1)),]


rownames(data.1)=str_replace(rownames(data.1), "HMA ", "")
rownames(data.1)=str_replace(rownames(data.1), "LMA ", "")


annotation_row=data.frame(Status=c(rep("HMA", 19), rep("LMA", 17)), stringsAsFactors = F)
rownames(annotation_row)= rownames(data.1)


#change names
colnames(data.1)[colnames(data.1)=="01phylum"]="Phylum"

colnames(data.1)[colnames(data.1)=="02class"]="Class"


#
pdf(paste("R4!.heatmap_", n, ".pdf", sep=""), width = 4, height = 7 )
par(mfrow=c(3,1))
pheatmap(data.1, color = rampcols, cluster_rows = F, cluster_cols = F, annotation_row =annotation_row, cellwidth =  10)
dev.off()

