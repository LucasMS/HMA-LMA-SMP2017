import glob
import pandas as pd
import numpy as np
from sklearn.ensemble import AdaBoostClassifier
import sys, os
from collections import Counter
from sklearn.cross_validation import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from sklearn.cross_validation import cross_val_score
from sklearn.cross_validation import StratifiedKFold
from collections import Counter
import sklearn.metrics


##Get files
lista_train= glob.glob("./Data/*train.csv")

lista_train=sorted(lista_train)




########Define classifiers

names = ["Nearest Neighbors", "Linear SVM", "RBF SVM", "Decision Tree",
         "Random Forest", "AdaBoost", "Naive Bayes", "Linear Discriminant Analysis",
         "Quadratic Discriminant Analysis"]
classifiers = [
    KNeighborsClassifier(3),
    SVC(kernel="linear", C=0.025),
    SVC(gamma=2, C=1),
    DecisionTreeClassifier(max_depth=5),
    RandomForestClassifier(max_depth=5, n_estimators=10, max_features=1),
    AdaBoostClassifier(),
    GaussianNB(),
    LinearDiscriminantAnalysis(),
    QuadraticDiscriminantAnalysis()]
 

fout=open("R2.Classifiers.sel.out", "w") 

for name, clf in zip(names, classifiers):
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
   #####DEBUG   
   #if sp == 'Ectyoplasia ferox':
    #print set(metadata_train[idx].loc[:,"Species"])#     
   ##########End of debug!!!!!

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
   fout.write(sp + "\t"+ str(score)+"\t"+ str(percentage_score)+"\t"+str(score_f1)+"\t"+i.split("/")[2].split(".")[0]+"\t" + str(name) + "\n") #ADD another score
  
fout.close()

