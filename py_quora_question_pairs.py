#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 29 16:04:07 2017

@author: peterweber
"""

import pandas as pd
from collections import Counter

#%%

df_train = pd.read_csv("~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/data/train.csv")
df_test  = pd.read_csv("~/Projects/Kaggle/Kaggle_QuoraQuestionPairs/data/test.csv")

#%%
train_qs = pd.Series(df_train['question1'].tolist() + df_train['question2'].tolist()).astype(str)
words = (" ".join(train_qs)).lower().split()
counts = Counter(words)
weights = {word: get_weight(count) for word, count in counts.items()}