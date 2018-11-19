
load("../../../Data/v6/3.b.Make.final/hl.v6.r.taxa.RData")

datas=c("smp.r.p" ,"smp.r.c" ,"smp.r.o" ,"smp.r.f" ,"smp.r.g" ,"smp.r.s",  "smp.r")

meta=hl.r.taxa$meta.r

meta=meta[, c("sample_name", "host_scientific_name", "Status", "Region")]
#rename to fit on the old code
colnames(meta)= c("SampleID","Species","MicAbundance","Region")
rownames(meta)=meta$SampleID

#separate train and predict

meta.train=meta[meta$MicAbundance != "NA",]
meta.predict=meta[meta$MicAbundance == "NA",]

#write

write.csv(meta.predict, "./Data/01metadata.Predict.csv", row.names = T)
write.csv(meta.train, "./Data/01metadata.Train.csv", row.names = T)

###

#phylum

p=hl.r.taxa$smp.r.p

p.train=p[match(meta.train$SampleID ,rownames(p)),]
p.predict=p[match(meta.predict$SampleID ,rownames(p)),]

identical(rownames(p.train), meta.train$SampleID)
identical(rownames(p.predict), meta.predict$SampleID)

write.csv(p.train, "./Data/01phylum.train.csv",  row.names = T)
write.csv(p.predict, "./Data/01phylum.predict.csv",  row.names = T)


#class


c=hl.r.taxa$smp.r.c

c.train=c[match(meta.train$SampleID ,rownames(c)),]
c.predict=c[match(meta.predict$SampleID ,rownames(c)),]

identical(rownames(c.train), meta.train$SampleID)
identical(rownames(c.predict), meta.predict$SampleID)

write.csv(c.train, "./Data/02class.train.csv",  row.names = T)
write.csv(c.predict, "./Data/02class.predict.csv",  row.names = T)

#Otu
otu=hl.r.taxa$smp.r

otu.train=otu[match(meta.train$SampleID ,rownames(otu)),]
otu.predict=otu[match(meta.predict$SampleID ,rownames(otu)),]

identical(rownames(otu.train), meta.train$SampleID)
identical(rownames(otu.predict), meta.predict$SampleID)

write.csv(otu.predict, "./Data/03OTU.predict.csv",  row.names = T)
write.csv(otu.train, "./Data/03OTU.train.csv",  row.names = T)
