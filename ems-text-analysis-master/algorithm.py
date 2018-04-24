#!/usr/bin/python

import numpy as np
import pandas as pd
from decimal import Decimal

############ This is the input data #############
data = pd.read_csv("FAKE_DATA_heim.csv", encoding = 'latin-1') ## NEEDS TO BE MADE RELATIVE FILE PATH
data['PI'] = data['PI'].astype('str').str.lower() 
data['CN'] = data['CN'].astype('str').str.lower() 
################################################

## These two are the keyword libraries
od_pi_terms = pd.read_csv("od_pi_terms.csv", encoding = 'latin-1') ## NEEDS TO BE MADE RELATIVE FILE PATH
od_cn_words = pd.read_csv("od_cn_words.csv", encoding = 'latin-1') ## NEEDS TO BE MADE RELATIVE FILE PATH

## This is a short keyword list - could be stored in memory if you'd prefer
drugs = ["heroin", "fentanyl", "methadone", "narcan", "overdose", "heroine", "od"] 

## Functions
def logitreg(row):
    
    """ Performs manual logit via coefficent values determined prior in R
    
    Args:
        row - row for use in apply
    
    Values:
        piC - Primary.Impression coefficent
        cnC - Cheif.Narrative coefficient
        drugC - drug score coeffienct
        intC - model intercept
    
    """
    piC = 18.178563
    cnC = 0.121893
    drugC = 3.052074
    intC = -25.961060
    prob = 1 / (1 + np.exp(-(row['piS'] * piC + row['cnS'] * cnC + row['drugS'] * drugC + intC)))
    return(prob)
    
#### SCORING FOR PI
terms = od_pi_terms["Primary.Impression"].values
data['piS'] = data.apply(lambda x: int(x.PI in terms) if type(x.PI) == str else False, axis = 1) 

#### SCORING FOR CN
scoring_words = od_cn_words["word"].values.tolist() 

scores = [None] * len(data)

for i in range(0, len(data)): 
    words = data.iloc[i, 5].split(" ")
    x = list(set(words).intersection(scoring_words))
    score = sum(od_cn_words.loc[od_cn_words['word'].isin(x)].score)
    scores[i] = score
    
data['cnS'] = scores
  
#### SCORING FOR DRUGS

for i in range(0, len(data)): 
    words = data.iloc[i, 5].split(" ")
    scores[i] = len(list(set(words).intersection(drugs)))
    
data['drugS'] = scores

#### PERFORM LOGIT
prob_val = input("Enter probability value from 0 to 1. 'exs: 0.15, 0.75, 0.50 etc.\n: ")
print(prob_val)
data["probs"] = data.apply(logitreg, axis = 1) ## This would be the value to be adjusted for based on preference

### count of identified overdoses displayed out of the total # of records inputted
### then a csv of just the overdoses with the date of the run as the title


print(data['probs'])

for i in range(0,len(data['probs'])):
    x = Decimal(data['probs'][i])
    print(round(x,5))


##example record
##print(data['probs'][0])

