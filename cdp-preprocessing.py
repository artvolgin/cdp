# -*- coding: utf-8 -*-
"""
Created on Sun Oct 25 17:54:37 2020

@author: Artem
"""

# =============================================================================
# 0. Libraries
# =============================================================================
# General purpose
import pandas as pd
import numpy as np
import math
import random
import sklearn
import pickle
import seaborn as sns
import scipy.stats as ss
import os
from itertools import combinations
import re
import time
from datetime import datetime
import matplotlib.pyplot as plt
import scipy
import sklearn.preprocessing as pp
import itertools
import gc
import math
import json
import urllib
import requests
from bs4 import BeautifulSoup
from statistics import mode 
import sqlite3
from operator import or_
from functools import reduce # python3 required
import dateutil.parser
import spacy
import io
pd.options.mode.chained_assignment = None 


# =============================================================================
# 1. Create tables with questions number and name
# =============================================================================

# Corporations -- Climate Change -- 2020
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Corporations/Corporations Responses/Climate Change")
df = pd.read_csv("2020_Full_Climate_Change_Dataset.csv")
df_questions = df.loc[:,["question_number", "question_unique_reference"]]
df_questions = df_questions.drop_duplicates().reset_index(drop=True)
df_questions['question_number_float'] = df_questions['question_number'].apply(
    lambda n: float(re.sub('[A-Za-z]', '', n)))
df_questions = df_questions.sort_values('question_number_float').drop('question_number_float', 1)
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Questions")
# df_questions.to_excel("questions_corporations_climate.xlsx", index=False)

# Corporations -- Water Security -- 2020
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Corporations/Corporations Responses/Water Security")
df = pd.read_csv("2020_Full_Water_Security_Dataset.csv")
df_questions = df.loc[:,["question_number", "question_unique_reference"]]
df_questions = df_questions.drop_duplicates().reset_index(drop=True)
df_questions['question_number_float'] = df_questions['question_number'].apply(
    lambda n: float(re.sub('[A-Za-z]', '', n)))
df_questions = df_questions.sort_values('question_number_float').drop('question_number_float', 1)
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Questions")
# df_questions.to_excel("questions_corporations_water.xlsx", index=False)

# Cities -- 2020
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Cities/Cities Responses")
df = pd.read_csv("2020_Full_Cities_Dataset.csv")
df_questions = df.loc[:,['Question Number', 'Question Name']]
df_questions = df_questions.drop_duplicates().reset_index(drop=True)
temp = df_questions.iloc[[95, 120],:]
df_questions = df_questions.drop([95, 120])
df_questions['question_number_float'] = df_questions['Question Number'].apply(
    lambda n: float(re.sub('[A-Za-z]', '', n)))
df_questions = df_questions.sort_values('question_number_float').drop('question_number_float', 1)
df_questions = temp.append(df_questions)
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Questions")
# df_questions.to_excel("questions_cities.xlsx", index=False)

# =============================================================================
# 2. Cities - 2019: Table-type Questions
# =============================================================================

# Lets focus on Cities - 2019 for now.

# Load test dataset: Companies - Climate change - 2020
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Cities/Cities Responses")
# df_cities_20 = pd.read_csv("2020_Full_Cities_Dataset.csv")
df_cities_19 = pd.read_csv("2019_Full_Cities_Dataset.csv")
# df_cities_18 = pd.read_csv("2019_Full_Cities_Dataset.csv")
# Select only usefull questions
os.chdir("C:/Users/Artem/YandexDisk/CDP/data/Supplementary Data/Recommendations from CDP")
df_questions = pd.read_excel("CDP_recommendations_for_questions_to_focus_on.xlsx")
selected_questions = df_questions.loc[df_questions['Select'] == 1, '2019 Question number'].values
selected_questions = list(map(lambda x: x[1:], selected_questions))
df_cities_19 = df_cities_19[df_cities_19['Question Number'].apply(
    lambda q: q in selected_questions)]

# Remove rows with NA's
df_cities_19 = df_cities_19[df_cities_19['Response Answer'].notna()]
# Unify "Other" category
df_cities_19['Response Answer'] = df_cities_19['Response Answer'].apply(
    lambda x: "Other" if x.startswith("Other") else x)


# Function for preprocessing of large table questions

def preprocessTableQuestion(df, question_number):
    '''
    
    Parameters
    ----------
    df : pd.DataFrame
        Dataframe with survey results in long format.
    question_number : str
        Number of table question 

    Returns
    -------
    df_table : pd.DataFrame
        Dataframe with city-row as the unit of observation.

    '''
    
    # Select table
    df_table = df[df['Question Number'] == question_number]
    df_table = df_table.loc[:,['Account Number', 'Column Name', 'Row Number', 'Response Answer']]
    df_table.columns = ['city_id', 'col_name', 'row_number', 'response']
    df_table = df_table.astype({'city_id':str, 'row_number':str})
    df_table['city_row_col'] = df_table['city_id'] + '_' + df_table['row_number'] + '_' + df_table['col_name']
    # Remove NA's
    df_table = df_table[df_table['response'].notna()]
    # Indicate multiply-choice questions
    temp = df_table['city_row_col'].value_counts().reset_index()
    temp['col'] = temp['index'].apply(lambda x: x.split('_')[2])
    multple_questions = temp[temp['city_row_col'] > 1]['col'].unique()
    
    # Preprocessing for multiple choice question
    for i in range(len(multple_questions)):
        df_mq = df_table[df_table['col_name'].apply(lambda x: x in multple_questions[i])]
        df_table = df_table[df_table['col_name'].apply(lambda x: x not in multple_questions[i])]
        df_mq['response'] = df_mq['response'].apply(
            lambda x: "Other" if x.startswith("Other") else x)
        df_mq = df_mq.pivot(index="city_row_col", columns='response', values='response')
        df_mq = df_mq.apply(lambda row: list(row), 1)
        df_mq = df_mq.apply(lambda row: list(pd.Series(row)[~pd.isnull(row)]))
        df_mq = pd.DataFrame(df_mq).reset_index()
        df_mq.columns = ['city_row_col', 'response']
        df_mq['city_id'] = df_mq['city_row_col'].apply(lambda x: x.split("_")[0])
        df_mq['row_number'] = df_mq['city_row_col'].apply(lambda x: x.split("_")[1])
        df_mq['col_name'] = df_mq['city_row_col'].apply(lambda x: x.split("_")[2])
        # Append to the table
        df_table = df_table.append(df_mq)
    df_table.drop(columns={'city_row_col'}, inplace=True)
    
    # Create final table with city-row as the unit of observation
    df_table['city_row'] = df_table['city_id'] + "_" + df_table['row_number']
    df_table = df_table.pivot(index="city_row", columns='col_name', values='response')
    df_table.reset_index(inplace=True)
    df_table['city_id'] = df_table['city_row'].apply(lambda x: x.split("_")[0])
    df_table['row_number'] = df_table['city_row'].apply(lambda x: x.split("_")[1])
    
    return df_table


# Preprocessing of C2.1, C2.2, C3.0, C5.0a, C6.0
df_c2_1 = preprocessTableQuestion(df_cities_19, "2.1")
df_c2_2 = preprocessTableQuestion(df_cities_19, "2.2")
df_c3_0 = preprocessTableQuestion(df_cities_19, "3.0")
df_c5_0a = preprocessTableQuestion(df_cities_19, "5.0a")
df_c6_0 = preprocessTableQuestion(df_cities_19, "6.0")
# TODO: Combine Hazards based on the common cause


### --- C2.1
# Climate Hazards
df_c2_1['Climate Hazards'].value_counts()
# Affected Services
temp = df_c2_1['Most relevant assets / services affected overall']
temp = temp[temp.notna()]
pd.Series(np.concatenate(temp.values)).value_counts()
# Vulnerable Populations
temp = df_c2_1['Please identify which vulnerable populations are affected']
temp = temp[temp.notna()]
pd.Series(np.concatenate(temp.values)).value_counts()
# Overall Social Impact
temp = df_c2_1['Social impact of hazard overall']
temp = temp[temp.notna()]
pd.Series(np.concatenate(temp.values)).value_counts()

### Importance of the Hazard
# 1. Probability
df_c2_1['Current probability of hazard'].value_counts()
df_c2_1['hazard_probability'] = df_c2_1['Current probability of hazard'].replace(
    {"High":10,
     "Medium High":8,
     "Medium":6,
     "Medium Low":4,
     "Low":2,
     "Does not currently impact the city":0,
     "Do not know":np.nan})
# 2. Consequences
df_c2_1['Current consequence of hazard'].value_counts()
df_c2_1['hazard_consequence'] = df_c2_1['Current consequence of hazard'].replace(
    {"High":10,
     "Medium High":8,
     "Medium":6,
     "Medium Low":4,
     "Low":2,
     "Does not currently impact the city":0,
     "Do not know":np.nan})
# 3. Previous impact
df_c2_1['Did this hazard significantly impact your city before 2019?'].value_counts()
df_c2_1['hazard_previous_impact'] = df_c2_1['Did this hazard significantly impact your city before 2019?'].replace(
    {"Yes":10,
     "No":0,
     "Do not know":np.nan})
# 4. Future change in frequency
df_c2_1['Future change in frequency'].value_counts()
df_c2_1['hazard_future_frequency'] = df_c2_1['Future change in frequency'].replace(
    {"Increasing":10,
     "Decreasing":5,
     "Not expected to happen in the future":0,
     "None":0,
     "Do not know":np.nan})
# 5. Future change in intensity
df_c2_1['Future change in intensity'].value_counts()
df_c2_1['hazard_future_intensity'] = df_c2_1['Future change in intensity'].replace(
    {"Increasing":10,
     "Decreasing":5,
     "Not expected to happen in the future":0,
     "None":0,
     "Do not know":np.nan})
# 6. Future impact
df_c2_1['Magnitude of expected future impact'].value_counts()
df_c2_1['hazard_future_impact'] = df_c2_1['Magnitude of expected future impact'].replace(
    {"High":10,
     "Medium":5,
     "Low":0,
     "Do not know":np.nan})
# 7. Term
df_c2_1['When do you first expect to experience those changes?'].value_counts()
df_c2_1['hazard_term'] = df_c2_1['When do you first expect to experience those changes?'].replace(
    {"Immediately":10,
     "Short-term (by 2025)":5,
     "Medium-term (2026-2050)":0,
     "Long-term (after 2050)":np.nan})
# Calculate Importance of the Hazard
hazard_columns = df_c2_1.columns[list(map(lambda x: x.startswith('hazard_'), df_c2_1.columns))]
hazard_means = df_c2_1.groupby('Climate Hazards')[hazard_columns].mean()
hazard_means['hazard_importance'] = hazard_means.sum(1)
hazard_means.reset_index(inplace=True)

# temp = df_c2_1.iloc[:,list(map(lambda x: x.startswith('hazard_'), df_c2_1.columns))]
# temp = df_c2_1.loc[:,['hazard_probability', 'hazard_consequence',
#                      'hazard_future_frequency', 'hazard_future_intensity']]
# temp.isna().sum(0)
# temp['NAs'] = temp.isna().sum(1)
# temp['NAs'].value_counts()


### Climate Hazards ~~~ Vunerable Populations
df_c2_1['Please identify which vulnerable populations are affected'].isna().sum()
df_harards_populations = pd.get_dummies(df_c2_1['Please identify which vulnerable populations are affected'].apply(pd.Series).stack()).sum(level=0)


# TODO: Association rules mining with Apriori

### --- C2.2 
# Factors that affect your city's ability to adapt to climate change
df_c2_2 = df_c2_2[df_c2_2['Factors that affect ability to adapt'].notna()]
# Filtering
df_c2_2 = df_c2_2[df_c2_2['Support / Challenge'] != 'Do not know']
temp = df_c2_2['Factors that affect ability to adapt'].value_counts()
temp = temp[temp < 10].index
df_c2_2['Factors that affect ability to adapt'] = df_c2_2['Factors that affect ability to adapt'].apply(
    lambda x: "Other" if x in temp else x)
df_c2_2['Factors that affect ability to adapt'].value_counts()
# Cross
cross_c2_2 = pd.crosstab(df_c2_2['Factors that affect ability to adapt'],
                         df_c2_2['Support / Challenge'], normalize="index")
# TODO: NLP to extract words which are helpfull to distinguishing between
# Support and Challenge, TF-IDF + LogReg


### --- C3.0
# Actions against climate change
df_c3_0 = df_c3_0[df_c3_0['Action'].notna()]
df_c3_0['Action'].value_counts() # "No action currently taken" category
# Co-benifit areas
temp = df_c3_0['Co-benefit area']
temp = temp[temp.notna()]
pd.Series(np.concatenate(temp.values)).value_counts()
# Merge with C2.1
df_c2_1['city_hazard'] = df_c2_1['city_id'] + '_' + df_c2_1['Climate Hazards']
df_c3_0['city_hazard'] = df_c3_0['city_id'] + '_' + df_c3_0['Climate hazards']
df_c2_1 = df_c2_1[df_c2_1['Climate Hazards'].notna()]
df_c3_0 = df_c3_0[df_c3_0['Climate hazards'].notna()]
df_c2_1_c3_0 = df_c2_1.merge(df_c3_0, on='city_hazard')
# Cross Hazards ~ Actions
cross_hazards_actions = pd.crosstab(df_c2_1_c3_0['Climate Hazards'],
                                    df_c2_1_c3_0['Action'])
# Filter hazards and actions with less than 10 observations
cross_hazards_actions = cross_hazards_actions[cross_hazards_actions.sum(1) > 10]
cross_hazards_actions = cross_hazards_actions.loc[:,cross_hazards_actions.sum(0) > 10]
# TODO: Diversity of actions for the hazard
cross_hazards_actions_bool = cross_hazards_actions > 10
cross_hazards_actions_bool.sum(1)
# TODO: Two-mode network: Hazards ~ Actions

### --- C5.0a
# TODO

### --- C6.0
# TODO

# =============================================================================
# 3. Cities - 2019: KPI
# =============================================================================

# TODO:






