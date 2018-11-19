library('pheatmap')
ncol=7#11
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
} 
cols <-c(add.alpha("dark red", alpha=0.7),"white",  add.alpha("dark blue", 0.7))
rampcols <- colorRampPalette(colors = cols, space="Lab")(ncol)


for (i in 1:10){
  dat=paste('./Prediction/Prediction', '.tsv', sep=as.character(i))
  dat=read.delim(dat, row.names = 1)
  data.m=as.matrix(dat)
  data.m=data.m[,c(1,3)]
  colnames(data.m)
  
  
  if (i == 1){
    d1P=data.frame(data.m[,1])
    colnames(d1P)[i]=as.character(i)
    d1C=data.frame(data.m[,2])
    colnames(d1C)[i]=as.character(i)
  }
  
  if (i != 1){
    d1P=cbind(d1P, data.m[,1])
    colnames(d1P)[i]=as.character(i)
    d1C=cbind(d1C,data.m[,2])
    colnames(d1C)[i]=as.character(i)
  }
}

data.m=data.frame(Phylum=rowMeans(d1P), Class=rowMeans(d1C))
##


data.m=as.matrix(data.m)




# library(mclust)
# Ys = data.frame((data.m))
# 
# fit <- Mclust(Ys)
# 
# plot.Mclust(fit, "BIC")
# 
# fit$G
# 
# Y.pca = princomp(Ys)
# 
# plot(Y.pca$scores[,1:2], col = fit$classification, pch = 19)
# 
# 
# require(vegan)
# fit <- cascadeKM(data.m, 1, 10, iter = 1000)
# plot(fit, sortg = TRUE, grpmts.plot = TRUE)
# calinski.best <- as.numeric(which.max(fit$results[2,]))
# cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")


library(apcluster)


#Create distance, The choice is the standard similarity measure used in the papers of Frey and Dueck — negative squared distances.
d=negDistMat(data.m, r=2)
#Create apreresult object.  Also, without a q set, the exemplars preferences were set to the median, which is expected to result in a moderate number of clusters in comparison of a small number of clusters that results when the exemplar preferences are set to their minimum (q=0). 
apres.d=apcluster(d)

apres.d

#Vizualize clusters
plot(apres.d, data.m)

#Vizualize details. It uses the median rule to determine input preferences (number of clusters)
apres.d=apcluster(d, details=T)
#Check how it was done. Net similrity has to converge and the objective of the alorithm is to inclrease net similarity

plot(apres.d)
#See heatmap with similarity matrix
heatmap(apres.d, d)



#It is a bit tricky to find the best number of clusters. Exemplar based agglomerative clustering (aggExCluster) on affinity propagation results provides an additional tool for finding the right number of clusters. The heatmao plot for affinity propagation results uses aggECcluster internally to arrange clusters ;). So, I will recover the clusters from there.

#Aggregate the clusters
aggres.ad=aggExCluster(d, apres.d)

aggres.ad

pdf('R7prediction.clusters.pdf')
plot(aggres.ad, ylab= 'Negative squared Euclidean distances', main=NA)
dev.off()

aggres.ad@order
#Aggregate the samples

#Now, get the clusters created by affinity propagation and order according to the results of agglomerative clustering (which made an hierarchical cluster of the AP clusters)


nomes <- data.frame(sp=character(),
                      cluster=numeric(),
                      stringsAsFactors=FALSE) 

for (i in aggres.ad@order){
  #for (z in  names(apres.d@clusters[[i]])){ #order by cluster
  for (z in  sort(names(apres.d@clusters[[i]]))){ #order by names
    datinha <- data.frame(sp=z,
                        cluster=i,
                        stringsAsFactors=FALSE)
   nomes=rbind(nomes, datinha) 
  }
  
}

annotation_row=data.frame(Cluster=as.character(nomes$cluster))
rownames(annotation_row)=nomes$sp

##Now, use this to plot
data.m=data.m[match(nomes$sp, rownames(data.m)),]



pdf('R7prediction.pdf', width=6, height=15)
par(mfrow=c(1,2))
pheatmap(data.m, color = rampcols, cluster_rows = F, cluster_cols = F, cex=0.9,border_color="white",annotation_row = annotation_row, cellwidth =  10)
plot(aggres.ad, horiz=T, main=NA,showSamples=T, ticks=3, cex.axis=0.7)
dev.off()

#pdf('R7.clusters.pdf', width=3, height=2)
#plot(aggres.ad, main=NA, las=2, cex.axis=0.3, ticks=3)
#dev.off()



write.table(nomes, "R7prediction.ap.clusters.tsv", sep="\t", quote=F, col.names = F, row.names = F)

