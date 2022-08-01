import pandas as pd
import os
import statistics
import matplotlib.pyplot as plt
import seaborn as sb
import chart_studio.plotly as py
import plotly.graph_objs as go 
import numpy as np
from sklearn.decomposition import PCA as sk_pca
from sklearn.preprocessing import StandardScaler

filename = "winequality-red.csv" 
filename2 = "winequality-white.csv" 
vinos_tintos = pd.read_csv(filename,sep=';')
vinos_blancos = pd.read_csv(filename2,sep=';')
print(vinos_tintos.shape)
print(vinos_blancos.shape)
atributos=vinos_tintos.columns.values
vinos_tintos[atributos[0:4]].describe()
vinos_tintos[atributos[4:8]].describe()
vinos_tintos[atributos[8:12]].describe()
vinos_blancos[atributos[0:4]].describe()
vinos_blancos[atributos[4:8]].describe()
vinos_blancos[atributos[8:12]].describe()




pd.isnull(vinos_blancos).values.ravel().sum()
pd.isnull(vinos_tintos).values.ravel().sum()






figure,axs = plt.subplots(1,1, sharey=True, sharex=True,figsize=(10,15))
sb.heatmap(vinos_blancos.corr(), cmap="YlGnBu", annot=True,ax=axs[0])




