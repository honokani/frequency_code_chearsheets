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
def findFileByExt(fPath=getCurrDirPath(), ext=""):
    _, files = getContetsNames(fPath)
    if (ext==""):
        return files
    else:
        founds = []
        ext = ext.lower()
        tgt = re.compile("\."+ext+"$")
        for f in files:
            if (not None==tgt.search(f.lower())):
                founds.append(f)
        return founds

def findFileByFullname(fPath=getCurrDirPath(), fullname=""):
    _, files = getContetsNames(fPath)
    if (fullname==""):
        return files
    else:
        founds = []
        fullname = fullname.lower()
        tgt = re.compile("^{}$".format(fullname))
        for d in dirs:
            if (not None==tgt.search(d.lower())):
                founds.append(d)
        return founds

def findDirByFullname(fPath=getCurrDirPath(), fullname=""):
    dirs, _ = getContetsNames(fPath)
    if (fullname==""):
        return dirs
    else:
        founds = []
        fullname = fullname.lower()
        tgt = re.compile("^{}$".format(fullname))
        for f in files:
            if (not None==tgt.search(f.lower())):
                founds.append(f)
        return founds


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# std input
def inputList(sp=" "):
    l = input()
    return l.split(sp)

def inputList_noEmpStr(sp=" "):
    return list(filter(lambda x:not x=="", inputList(sp)))

def inputSet_withLen():
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

def inputLists_withLen():
    mayDigit = input()
    if (mayDigit.isdigit() and mayDigit.isalnum()):
        n = int(mayDigit)
        if (0<n):
            return n, list(map(lambda _: inputList_noEmpStr(), range(n)))
        else:
            print("Negative!")
            return n, []
    else:
        print("NaN!")
        return mayDigit, []

def inputLists_untilEnd():
    ls = []
    while True:
        l = input()
        if (l==""):
            return ls
        else:
            ls.append(list(filter(lambda x:not x=="", l.split(" "))))

