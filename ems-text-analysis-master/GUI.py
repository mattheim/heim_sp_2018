#!/usr/bin/python

import tkinter
from tkinter import messagebox

def mainFrame(root):
    mainFrame = tkinter.Frame(root, height=300, width=600, bd=5, bg='#33FFFF')
    mainFrame.pack()
    return mainFrame

def listbox1(mainFrame):

    lb1 = tkinter.Listbox(mainFrame,height=275,width=250, bg='white')
    lb1.pack()
    lb1.place(anchor='nw', height=275, width=250)

    return lb1

def listbox2(mainFrame):

    lb2 = tkinter.Listbox(mainFrame,height=275,width=250, bg='white')
    lb2.pack()
    lb2.place(relx=1, x=0, y=-1, anchor='ne', height=275, width=250)

    return lb2

def runButton(mainFrame):

    rb = tkinter.Button(mainFrame,text="Click me",)
    rb.pack()
    rb.place(relx=0.5, rely=0.5, anchor='s',height=50,width=75)

    return rb

def main():

    root = tkinter.Tk()
    root.resizable(False, False)

    #set up main Frame
    mf = mainFrame(root)

    #set up list 1 inside the mainFrame
    listbox1(mf)

    #set up list 2 inside the mainFrame
    listbox2(mf)

    #set up run button inside the mainFrame
    runButton(mf)

    root.mainloop()

main()

