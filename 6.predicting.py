import glob
import sys, os
import pandas as pd
import numpy as np
from sklearn.svm import SVC
from collections import Counter
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier

#No Erylus and Etyoplasia on the training data



lista_train= glob.glob("./Data/*train.csv")
lista_train=sorted(lista_train)
lista_train=lista_train[:-1] #remove OTUS

#lista_train=['./Data/01phylum.train.csv']

dic_data={}




#clf=SVC(gamma= 0.032, C= 3.162)
#clf=AdaBoostClassifier(n_estimators=300)
clf=RandomForestClassifier(max_depth=None, n_estimators=50, max_features="auto")
metadata_train= pd.read_csv(open('./Data/01metadata.Train.csv'), index_col =0)
metadata_train.index=map(str, metadata_train.index)


metadata_predict= pd.read_csv(open('./Data/01metadata.Predict.csv'))
metadata_predict.index=map(str, metadata_predict.index)

species=sorted(list(set(metadata_predict["Species"])))
dic_data={}

#k=species[0]

for k in species:
 dic_data[k]={}
# Do not intend after here

for i in sorted(lista_train):
#i=lista_train[0]



#Intend after here 1

#Open files
 data_train = pd.read_csv(open(i), index_col =0, dtype={'user_id':pd.np.int64}, low_memory=False)
 data_train.index=map(str, data_train.index)

 data_predict = pd.read_csv(open(i.split(".train")[0]+".predict.csv"), index_col =0, dtype={'user_id':pd.np.int64}, low_memory=True) #Changed from False to True from the original v5/v7 because of large dataset
 data_predict.index=map(str, data_predict.index)   

#train
 X_train=data_train.values
 y_train=metadata_train.loc[:,"MicAbundance"]#Need to get out of pandas

 clf.fit(X_train, y_train)


 for sp in species:
#sp=species[0]

#Add intent 2



#select from the metadata the sps that are matching with the the species of the round
  sp_selected=metadata_predict.loc[metadata_predict["Species"] == sp]
  sp_sample_names=(sp_selected.index)
  sp_sample_names=map(int,sp_sample_names)
    
#keep in the _test only names selected
  X_predict=data_predict.iloc[sp_sample_names,:].values


  prediction = clf.predict(X_predict) #

  count=Counter(prediction)
  soma=sum(count.values())
  dic_data[sp][i]={"HMA":count["HMA"], "LMA":count["LMA"], "PHMA": count["HMA"]* 100/soma, "PLMA": count["LMA"]* 100/soma}




#fout=open("R6prediction.Filtered.tsv",  "w")

fout=open("./Prediction/Prediction"+sys.argv[1]+".tsv", "w")#open("R3.out", "w") 


fout.write("Species")
for i in sorted(lista_train):
 fout.write("\t" + i.split(".train")[0]+ "_pctHMA"  "\t" + i.split("/")[2].split(".")[0]+ "_pctLMA")
fout.write("\n")
    

for k in dic_data.keys():
 fout.write( k ) 
 for v in sorted(dic_data[k].keys()):
  fout.write("\t" + str(dic_data[k][v]["PHMA"]) + "\t" + str(-dic_data[k][v]["PLMA"]))
 fout.write( "\n")   
           
fout.close()
