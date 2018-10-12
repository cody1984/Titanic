""" Feature engineering of Titanic data set"""

import pandas as pd
import numpy as np

train = pd.read_csv('train.csv')

df = train

# print(df.head(10))

# https://stackoverflow.com/questions/19913659/pandas-conditional-creation-of-a-series-dataframe-column
# New column to show group status
conditions = [
    (df['SibSp'] > 0),
    (df['Parch'] > 0)]
choices = ['yes', 'yes']
df['Group'] = np.select(conditions, choices, default='no')

# New column to show cabin status
df['hasCabin'] = np.where(df['Cabin'] >= "", 'yes', 'no')

# New column to show ageGroup status
conditions = [
    (df['Age'] >= 18),
    (df['Age'] < 18) & (df['Age'] >= 13),
    (df['Age'] < 13) & (df['Age'] >= 2),
    (df['Age'] < 2)]
choices = ['adult', 'teen', 'child', 'infant']
df['ageGroup'] = np.select(conditions, choices, default='none')

conditions = [
    (df['ageGroup'] == 'none') & (df['Name'].str.contains(r"Mr")),
    (df['ageGroup'] == 'none') & (df['Name'].str.contains(r"Mrs"))]
choices = ['adult', 'adult']
df['ageGroup'] = np.select(conditions, choices, default=df['ageGroup'])

df.to_csv('train_py.csv')

#print(df.head(20))

