# csv
import csv
def csv2List(file_path):
    r = []
    with open(file_path, 'r') as f:
        reader = csv.reader(f)
        r = list(reader)
    return r

def csv2List_removeEmpty(file_path):
    return list(filter(lambda x:not x==[], csv2List(file_path)))




# directory
import os
def getCurrFilePath():
    return os.path.realpath(__file__)

def getCurrDirPath():
    return os.path.dirname(getCurrFilePath())

import glob
def getNameList_in(file_path):
    fNames = glob.glob(os.path.join(file_path,"*"))








