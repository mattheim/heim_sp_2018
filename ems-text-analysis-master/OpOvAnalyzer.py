#!/usr/bin/python

import tkinter
import numpy as np
import pandas as pd
import csv
import datetime
from decimal import Decimal
import os
from tkinter import messagebox


def probSlider(mainFrame):

    w = tkinter.Scale(mainFrame, from_= 0.0, to=100, orient=tkinter.HORIZONTAL)
    w.pack()
    w.place(relx=0.51, rely=0.85, anchor='s', height = 50, width = 100)

def mainFrame(root):
    mainFrame = tkinter.Frame(root, height=300, width=600, bd=5, bg='grey')
    mainFrame.pack()
    return mainFrame

def listbox1(mainFrame):

    lb1 = tkinter.Listbox(mainFrame,height=275,width=250, bg='white')
    lb1.pack()
    lb1.place(anchor='nw', height=275, width=200)

    return lb1

def listbox2(mainFrame):

    lb2 = tkinter.Listbox(mainFrame,height=275,width=250, bg='white')
    lb2.pack()
    lb2.place(relx=1, x=0, y=-1, anchor='ne', height=275, width=200)

    return lb2

def runButton(mainFrame):

    rb = tkinter.Button(mainFrame,text="Run",command= runAlgorithm)
    rb.pack()
    rb.place(relx=0.5, rely=0.5, anchor='s',height=50,width=75)

    return rb

def runAlgorithm():
    ############ This is the input data #############
    data = pd.read_csv("FAKE_DATA_heim.csv", encoding='latin-1')  ## NEEDS TO BE MADE RELATIVE FILE PATH
    data['PI'] = data['PI'].astype('str').str.lower()
    data['CN'] = data['CN'].astype('str').str.lower()
    ################################################

    ## These two are the keyword libraries
    od_pi_terms = pd.read_csv("od_pi_terms.csv", encoding='latin-1')  ## NEEDS TO BE MADE RELATIVE FILE PATH
    od_cn_words = pd.read_csv("od_cn_words.csv", encoding='latin-1')  ## NEEDS TO BE MADE RELATIVE FILE PATH

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
        return (prob)

    #### SCORING FOR PI
    terms = od_pi_terms["Primary.Impression"].values
    data['piS'] = data.apply(lambda x: int(x.PI in terms) if type(x.PI) == str else False, axis=1)

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
    ##prob_val = input("Enter probability value from 0 to 1. 'exs: 0.15, 0.75, 0.50 etc.\n: ")
    prob_val = 0.5

    data["probs"] = data.apply(logitreg, axis=1)  ## This would be the value to be adjusted for based on preference

    ### count of identified overdoses displayed out of the total # of records inputted
    ### then a csv of just the overdoses with the date of the run as the title
    probsList = []
    probsIndexList = []
    csvFileArray = []
    csvFileArrayFinal = []

    for i in range(0, len(data['probs'])):
        x = Decimal(data['probs'][i])
        x = ((round(x, 5)))
        if Decimal(x) >= Decimal(prob_val):
            print(x, ' ', i)
            probsList.append(x)
            probsIndexList.append(i)

    with open('FAKE_DATA_heim.csv', newline='') as f:
        reader = csv.reader(f)
        for row in csv.reader(f, delimiter=','):
            csvFileArray.append(row)

    f.close()

    for item in probsIndexList:
        csvFileArrayFinal.append(csvFileArray[item + 1])

    now = datetime.datetime.now()
    title = now.strftime("%Y-%m-%d")

    my_df = pd.DataFrame(csvFileArrayFinal)
    my_df.to_csv(title + ".csv", index=False, header=False)
    messagebox.showinfo("Title", "success")

def main():

    root = tkinter.Tk()
    root.resizable(False, False)

    #set up main Frame
    mf = mainFrame(root)

    #set up list 1 inside the mainFrame
    listbox1(mf)
    #set up list 2 inside the mainFrame
    listbox2(mf)
    #probability slider inside the mainFrame
    probSlider(mf)
    # set up run button inside the mainFrame
    runButton(mf)

    root.mainloop()

main()

