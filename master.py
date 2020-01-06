# importing required packages
import pandas as pd
import pandas_profiling
import numpy as np
import seaborn as sns
from scipy import stats
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from scipy.spatial.distance import cdist
import math
import more_itertools as mit

#model libraries
from sklearn import preprocessing, svm, metrics
from sklearn.feature_selection import SelectKBest, chi2
from sklearn.feature_selection import f_classif
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split, StratifiedKFold
from imblearn.over_sampling import SMOTE, ADASYN
from sklearn.ensemble import (RandomForestClassifier, AdaBoostClassifier, 
RandomForestRegressor, ExtraTreesClassifier,  GradientBoostingClassifier, BaggingClassifier)
from sklearn.linear_model import LogisticRegression
from sklearn import tree
from sklearn.feature_selection import RFE
from sklearn.neighbors import KNeighborsClassifier
from scipy.stats import mstats, stats
from sklearn.ensemble import VotingClassifier
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.impute import SimpleImputer
from sklearn.model_selection import cross_val_score
from scipy.spatial.distance import cdist
from sklearn.metrics import roc_curve, roc_auc_score
from sklearn.svm import SVC, LinearSVC
from sklearn.naive_bayes import GaussianNB, BernoulliNB
from sklearn import linear_model
from sklearn.gaussian_process import GaussianProcessClassifier
from sklearn.gaussian_process.kernels import RBF
from sklearn.neural_network import MLPClassifier

# importing the data
df = pd.read_csv('telecom_churn.txt',sep='\t')
df.columns = [c.replace(' ', '_') for c in df.columns]

#Profile report
profile = df.copy().profile_report(title='Pandas Profiling Report')


#Check dtypes
print(df.dtypes)

#remove strings from a numeric column
def ConvNum(col):
     return(pd.to_numeric(col, errors='coerce'))

#Find columns that are binary
def CheckBinary(df):
    flaglist=[]
    checkFlag = df.mode(numeric_only=True).head(1)
    for i in checkFlag.columns:
        if checkFlag[i][0] == 0 or checkFlag[i][0]==1:
            flaglist.append(i)
    return flaglist

#Find highly unique columns
def CheckUnique(df):
    unq_list = []
    unq = df.nunique()/len(df)
    for i in unq:
        if i >= 0.98:
            unq_list.append(unq[unq==i].idxmax())
    
    return unq_list

#Outlier removal
def RemOutlier(df):
    print("Z-score method")
    numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
    cols = list(df.select_dtypes(include=numerics).columns)
    getFlags = CheckBinary(df[cols])
    cols = set(cols) - set(getFlags)
    unq_cols = CheckUnique(df[cols])
    cols = cols - set(unq_cols)
    newdf = df[cols]
    newdf = newdf[(np.abs(stats.zscore(newdf)) < 3).all(axis=1)]
    df = df[set(df.columns)-set(cols)].join(newdf,how='inner')
    return df, getFlags, unq_cols


def categorical_summarized(dataframe, x=None, y=None, hue=None, palette='Set1', verbose=True):
    if x == None:
        column_interested = y
    else:
        column_interested = x
    series = dataframe[column_interested]
    print(series.describe())
    print('mode: ', series.mode())
    if verbose:
        print('='*80)
        print(series.value_counts())

    sns.countplot(x=x, y=y, hue=hue, data=dataframe, palette=palette)
    plt.show()



df['Night_Calls'] = df['Night_Calls'].apply(ConvNum)

#numeric columns
#This'll include "Flags" - that can be considered as categorical
numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
print(df.select_dtypes(include=numerics))

#categorical columns
#sample
print(df.select_dtypes(exclude=numerics))


#Check for nan
cols = df.columns[df.isna().any()].tolist()
print(cols)
df[cols] = df[cols].fillna(np.mean) #any other approach would also work

#remove outliers
df, binaryCols, unq_cols = RemOutlier(df)
print(binaryCols, unq_cols)

#numeric, catrgorical and Flags
num_cols = df.select_dtypes(include=numerics).columns
flags = CheckBinary(df[num_cols])
num_cols = set(num_cols) - set(flags)
categ_cols = set(list(df.columns)) - num_cols
categ_cols = categ_cols - set(unq_cols) - set(CheckUnique(df))

#CountPlots
fig, ax = plt.subplots(2,2)
fig.suptitle("Count Plots", fontsize="x-large")
x = y = 0
for i in categ_cols:
    sns.countplot(data=newdf, x=i, ax=ax[x][y])
    y+=1
    if y==2:
        x, y = x+1, 0
    if x==2:
        x = y = 0
        fig, ax = plt.subplots(2,2)
        fig.suptitle("Count Plots", fontsize="x-large")
        
        
        
#basic functions (python)
a = [1,2,3]
print(a[0:2]) 
print(a[0::2])#2 elements from 1st position

a.extend([5])
print(a)

df.describe() #summary stats
df.info() #null, non-null info

df.sort_values(by=['State','Night_Mins'],ascending=True,inplace=False) #sort in pandas

df.drop_duplicates() #drop dups
df.drop_duplicates(['State','Churn'])

#lambda functions
df['Churn_Flag'] = df['Churn'].apply(lambda x: "True" if x==1 else "False")

#replace
data = pd.Series([1., -999., 2., -999., -1000., 3.])
data.replace([-999,-1000],np.nan,inplace=True)

#rename
df.rename(columns={'Churn_Flag':'ChurnF'})

#value counts
pd.value_counts(df['Churn_Flag'])


#groupby
df.groupby(['Churn','VMail_Plan'])['Night_Mins'].mean()

#slicing
df.iloc[0:5] #rows
df.iloc[3] #4th row - index = 3

df.loc[:,'Churn':'VMail_Plan'] #set of columns

df.ix[:,15:] #slice columns


df[df['State'].isin(['OH','KS'])] #isin

df.query('Night_Mins > Eve_Mins | Eve_Mins < Day_Mins') #where A > B or B < C

#another way of grouping by
df.pivot_table(index=["Int'l_Plan","VMail_Plan"], columns='Churn', values=["Account_Length"], aggfunc="count",margins=True).reset_index()

df.shape #shape of dataframe

df.isnull().sum() #no. nulls total

df.dropna() #drop NA



#Models


#Pre-model functions

#Correlated columns
#X - Dataframe
def correlation(X,heatmap=False):
    corr = X.corr(method="pearson").abs()    
    upper = corr.where(np.triu(np.ones(corr.shape), k=1).astype(np.bool))    
    # Find features with correlation greater than 0.95
    to_drop = [column for column in upper.columns if any(upper[column] > 0.80)]
    X = X.drop(to_drop,axis=1)

    if heatmap==True:
        sns.heatmap(corr[(corr >= 0.5) | (corr <= -0.4)], 
            cmap='viridis', vmax=1.0, vmin=-1.0, linewidths=0.1,
            annot=True, annot_kws={"size": 8}, square=True)

    return X

#Significance Testing
#KruskalWallis - for categorical data - This changes depending on the problem and the function will change as well
def sigTest(X):
    
    sig_list = []
    for col in X:
            H, pval = mstats.kruskalwallis(list(X[col]),list(y['col'])) #y['col'] is the predictor variable
            if pval < 0.05:
                sig_list.append(col)
    
    X = X[sig_list]
    return X

#Feature Selectin Example 
#Top 20 Features by default - Works with RandomForestClassifier
    
#THis varies depending on the given case study
#Useful link - https://www.analyticsvidhya.com/blog/2016/12/introduction-to-feature-selection-methods-with-an-example-or-how-to-select-the-right-variables/
    
def featSelect(X, y, model, topN=20):
    model.fit(X,y)
    feat_importances = pd.Series(model.feature_importances_, index=X.columns)
    feat_importances = feat_importances.nlargest(topN).to_frame(name='values')
    feat_importances.reset_index(level=0, inplace=True) 
    
    cols = feat_importances['index'].values
    X = X[cols]
    return X, feat_importances

#Model Evaluation

#Split Train and Test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=42, stratify=y)

#Pre-model

X_train = correlation(X_train) #Remove highly correlated columns

X_train = sigTest(X_train) #Don't forget to change y['col'] to point to y_train predictor flag

#models
#Choose any
model = RandomForestClassifier(n_estimators=25,class_weight='balanced')
model = LogisticRegression(C=800, solver='lbfgs')
model = AdaBoostClassifier(learning_rate=0.9,base_estimator=LogisticRegression(penalty='l2',C=700))
model = GradientBoostingClassifier(learning_rate=0.0099,n_estimators=200)
model = RandomForestClassifier(n_estimators=100,min_samples_leaf=60,class_weight='balanced')
model = BaggingClassifier(n_estimators=50,base_estimator=LogisticRegression(penalty='l2',C=700),verbose=20)
model = BernoulliNB()
model = linear_model.SGDClassifier(max_iter=1000, tol=1e-3)
model = LinearSVC(penalty='l2',tol=1e-5)
kernel = 1.0 * RBF(1.0)

X_sel, feat_importances = featSelect(X_train, y_train['col'], model, 25)

#USE SMOTE for imbalance data
#sm =  SMOTE(random_state=20)
#x_res, y_res = sm.fit_sample(X_train, y_train['col'])

clf = model #chosen from above list or our own
clf.fit(X_train, y_train['col']) #Fit the model
output = clf.predict(X_test[X_train.columns])

#classification report
#Precision, recall, f-score
classif_report = metrics.classification_report(y_true=y_test,y_pred=output, output_dict=True)
classif_report = pd.DataFrame(classif_report).transpose()
conf_matrix = pd.crosstab(y_test['col'], output, rownames=['True'], colnames=['Predicted'], margins=True)
balanced_accuracy = (((conf_matrix[0][0] * 1.0)/conf_matrix['All'][0]) + ((conf_matrix[1][1] * 1.0)/conf_matrix['All'][1]))/2

#Auc-roc-score 
auc = roc_auc_score(y_test['CKD_Flag'], probs)
# calculate roc curve
fpr, tpr, thresholds = roc_curve(y_test['CKD_Flag'], probs)
plt.title('Receiver Operating Characteristic')
plt.plot(fpr, tpr, 'k', label = 'AUC = %0.2f' % auc)
plt.legend(loc = 'lower right')
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.grid(False)
plt.show()

#KMeans

#Elbow method Example
distortions = []
K = range(1,10)
for k in K:
    kmeanModel = KMeans(n_clusters=k).fit(df_clust)
    kmeanModel.fit(df_clust)
    distortions.append(sum(np.min(cdist(df_clust, kmeanModel.cluster_centers_, 'euclidean'), axis=1)) / df_clust.shape[0])

# Plot the elbow
plt.plot(K, distortions, 'bx-')
plt.xlabel('k')
plt.ylabel('Distortion')
plt.title('The Elbow Method showing the optimal k')
plt.show()

#Silhoutte score example
from sklearn.metrics import silhouette_score
range_n_clusters = list (range(2,10))
for n_clusters in range_n_clusters:
    clusterer = KMeans (n_clusters=n_clusters)
    preds = clusterer.fit_predict(df_clust)
    centers = clusterer.cluster_centers_
    score = silhouette_score (df_clust, preds, metric='euclidean')
    print (n_clusters, score)

#Get cluster 
km = KMeans(n_clusters=5 ,random_state=21).fit(df_clust)
cluster_map = pd.DataFrame()
cluster_map['data_index'] = df_clust.index.values
cluster_map['cluster'] = km.labels_
merged = pd.merge(df, cluster_map, left_index=True, right_on='data_index')


#PCA example

from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

sc = StandardScaler()  
merged3 = sc.fit_transform(merged2)  
merged3 = sc.transform(merged2)  
pca = PCA(n_components=10)
principalComponents = pca.fit_transform(merged3)
principalDf = pd.DataFrame(data = principalComponents
             , columns = ['pc1','pc2','pc3', 'pc4', 'pc5', 'pc6', 'pc7', 'pc8', 'pc9', 'pc10'])
print(pca.explained_variance_ratio_)
print(pca.components_)

