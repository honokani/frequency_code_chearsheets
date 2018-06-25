# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
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



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# directory
import os
def getCurrFilePath():
    return os.path.realpath(__file__)

def getCurrDirPath():
    return os.path.dirname(getCurrFilePath())

def getContetsNames(fPath=getCurrDirPath()):
    files = []
    dirs = []
    for tgt in os.listdir(fPath):
        if (os.path.isfile(os.path.join(fPath,tgt))):
            files.append(tgt)
        else:
            dirs.append(tgt)
    return dirs, files

import re
def findFileNames(fPath=getCurrDirPath(), ext=""):
    _, files = getContetsNames(fPath)
    if (ext==""):
        return files
    else:
        found = []
        ext = ext.lower()
        tgt = re.compile("\."+ext+"$")
        for f in files:
            if (not None==tgt.search(f.lower())):
                found.append(f)
        return found



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# std input
def inputList(sp=" "):
    l = input()
    return l.split(sp)

def inputList_noEmpStr(sp=" "):
    return list(filter(lambda x:not x=="", input2List(sp)))

def inputSet_byLen():
    mayDigit = input()
    if (mayDigit.isdigit() and mayDigit.isalnum()):
        n = int(mayDigit)
        if (0<n):
            return n, list(map(lambda _: input(), range(n)))
        else:
            print("Negative!")
            return n, []
    else:
        print("NaN!")
        return mayDigit, []


def inputSet_byLen_noEmpStr(sp=" "):
    return (lambda x,y: (x, list(filter(lambda z: not z=="", y))))(inputSet_byLen())






