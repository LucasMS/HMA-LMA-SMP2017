import glob
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
import sys, os
from collections import Counter
import sklearn.metrics

lista_train= glob.glob("./Data/*train.csv")

lista_train=sorted(lista_train)
lista_train=lista_train[:-1] #remove OTUS


iterations=range(0,110,10)
iterations=iterations[1:]

#iterations=range(5,10,2)#


fout=open("./4.1.random.forest/R4.1.all."+sys.argv[1]+".out", "w")

for ada in iterations:
 clf=RandomForestClassifier(max_depth=None, n_estimators=ada, max_features="auto")
 metadata_train= pd.read_csv(open('./Data/01metadata.Train.csv'), index_col =0)
 metadata_train.index=map(str, metadata_train.index)
 metadata_test= metadata_train


 species=sorted(list(set(metadata_test["Species"])))
 dic_data={}

 for k in species:
  dic_data[k]=[]   


 lista_train_data=[]
 for i in lista_train:
  lista_train_data.append(pd.read_csv(open(i), index_col =0, dtype={'user_id':pd.np.int64}, low_memory=False))



 for numero in range(0,len(lista_train)):
  i=lista_train[numero] #1
 
#Open files
  data_train = lista_train_data[numero]#pd.read_csv(open(i), index_col =0, dtype={'user_id':pd.np.int64})
  data_train.index=map(str, data_train.index)   
  data_test = data_train

#select the unique species



  for sp in species:
#sp=species[17]


#I
#select from the metadata the sps that are matching with the the species of the round
   sp_selected=metadata_test.loc[metadata_test['Species'] == sp]
   sp_sample_names=(sp_selected.index)

#keep in the _test only names selected
   y_test=metadata_test.loc[sp_sample_names,"MicAbundance"]#Need to get out of pandas
   X_test=data_test.loc[sp_sample_names,:].values

#II

#select from the metadata test the sps that are matching with the the species of the round
   sp_selected=metadata_train.loc[metadata_train['Species'] == sp]
   sp_sample_names=(sp_selected.index)
  



#keep in the _train everything, except names selected
   idx = data_train.index.isin(sp_sample_names) #get index of samples to exclue
   X_train=data_train[~idx].values

   y_train=metadata_train[~idx].loc[:,"MicAbundance"]#Need to get out of pandas

   #PERMUTATION!
   rng = np.random.RandomState(0) 
   permutation = rng.permutation(len(X_train))
   X_train, y_train = X_train[permutation], y_train[permutation]


   clf.fit(X_train, y_train)
   prediction = clf.predict(X_test) # accuracy
   score = clf.score(X_test, y_test) #Test samples, True labels #SEEMS TO BE THE SAME AS NUMBER OF CORRECTLY CLASSIFIED.
   score_f1=sklearn.metrics.f1_score(y_test, prediction, average="binary", pos_label=y_test[0])#Ground truth (correct) target values, Estimated targets as returned by a classifier. GET F1 just for the label being tested
   #Calculate percentage scores
   count=Counter(prediction)
   percentage={"HMA":count["HMA"], "LMA":count["LMA"]}
   percentage_score= percentage[y_test[0]]*100/len(y_test)
   
   #print score, percentage_score,score_f1, prediction, y_test
   

#write results    
   fout.write(sp + "\t"+ str(score)+"\t"+ str(percentage_score)+"\t"+str(score_f1)+"\t"+i.split("/")[2].split(".")[0]+"\t" + str(ada) + "\n") #ADD another score
  
fout.close()


