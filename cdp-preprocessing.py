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
# 2. Cities - 2020: Preporessing and EDA
# =============================================================================

# Load test dataset: Companies - Climate change - 2020
df = pd.read_csv("2020_Full_Climate_Change_Dataset.csv")





