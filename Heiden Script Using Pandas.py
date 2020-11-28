# -*- coding: utf-8 -*-
"""
I'm redoing my BUA633 final project in Python.
I still don't know how to do GAM (generalized additive model), or 
robust regressions.But I think I've pretty much replicated everything else
in the project, and I've gotten the basics down.
"""
import os
#import numpy as np
import seaborn as sns
from matplotlib import pyplot as plt
from statsmodels.formula.api import ols
#from statsmodels.gam.api import GLMGam, BSplines
#from statsmodels.sandbox.regression.predstd import wls_prediction_std
import pandas as pd

#loading the file

mainpath = "/Users/matthewheiden/Desktop/school/st johns/spring 2020/BUA 633 forecasting/"
path = os.chdir(mainpath)
air_data = pd.read_excel('Heiden BUA633 Final.xlsx')

#examining the data
print(air_data.head(15))

air_data_os = air_data.iloc[-2,:] #want row at obs = 242, all columns
air_data_is = air_data.iloc[:-2, :] # want the first 241 rows, all columns

#histograms of all variables

sns.distplot(air_data_is.ARPM, kde=False)
sns.distplot(air_data_is.unrate, bins=15, kde=False)
sns.distplot(air_data_is.RDPI, bins=10, kde=False)

# time series plots of all variables

sns.lineplot(x=air_data_is.observation_date, y=air_data_is.ARPM)
sns.lineplot(x=air_data_is.observation_date, y=air_data_is.unrate)
sns.lineplot(x=air_data_is.observation_date, y=air_data_is.RDPI)

# scatterplots of dependent variable (ARPM) vs independent variables (unrate and RDPI)
# Maybe there's a better scatterplot function, but this is ok for now

sns.regplot(x=air_data_is.unrate, y=air_data_is.ARPM)
sns.regplot(x=air_data_is.RDPI, y=air_data_is.ARPM)

#correlation matrix

CorrMatrix = air_data_is.iloc[:,1:5].corr()
print(CorrMatrix.round(3))

#summary statistics (Professor's app had skewness and kurtosis, but this basically works)
#the indexing below is to ignore the obs and month dummy variables
air_data_is.iloc[:,1:5].describe().round(3)[1:]

#linear regression
air_model = ols("ARPM ~ obs + unrate + RDPI + Feb + March + April + May + June + July\
                + August + Sept + Oct + Nov + Dec", data=air_data_is).fit()
air_model_summary = air_model.summary()
print(air_model_summary)

#predicting out of sample
air_predicted = air_model.predict()
air_model.predict().round()
air_model.predict(air_data).iloc[-2] #This is the value for Feb 2020

#histogram of the residuals

resid = air_model.resid
sns.distplot(resid, kde=False)

#scatterplot of actual vs. predicted
#you have to run all three lines simultaneously

ax = plt.figure(figsize = (15,8))
ax = sns.lineplot(x=air_data_is.observation_date, y=air_data_is.ARPM, label = 'Real Values')
ax = sns.lineplot(x=air_data_is.observation_date, y=air_predicted, label = 'Predicted Values')

#alternative way using matplotlib (still need to run everything simultaneously)
air_pred_df = pd.DataFrame({'observation_date':air_data_is.observation_date, 'Predicted':air_predicted})

ax = plt.figure(figsize = (10,8))
ax = plt.plot('observation_date', 'ARPM', label = 'Real Values', data=air_data_is)
ax = plt.plot('observation_date', 'Predicted', label = 'Predicted Values', color ='darkred', ls = '-.', data=air_pred_df)

#scatterplot of residuals with each independent variable

sns.regplot(x=air_data_is.unrate, y=resid)
sns.regplot(x=air_data_is.RDPI, y=resid)

