#!/usr/bin/python3

import re
import os
import argparse, sys

debugMode = False
def printLog (stringLog):
    if (debugMode):
        print ("[DEBUG] " + stringLog) 

def cleanUp(text):
    printLog("Cleaning Up")
    head = text.split("\n")
    head = ''.join([ ' >>' + i + "\n" for i in head[0:5]])
    printLog("Top: \n" + head)
    result = re.sub("\[OMPVV_INFO[a-zA-Z0-9.:_ ]*] ","", text)
    result = re.sub(" AVG_TIME = ","\t", result)
    result = re.sub(" us, (STD_DEV|MEDIAN|MAX_TIME|MIN_TIME) = ","\t", result)
    result = re.sub(" us","", result)
    result = re.sub("^[^[].*\n","", result)
    printLog("Cleaning Up done")
    head = result.split("\n")
    head = ''.join([' >>' + i + "\n" for i in head[0:5]])
    printLog("Top: \n" +  head)
    return result 

def main():
    ''' Arguments parsing'''
    parser = argparse.ArgumentParser(description="Changes the timing log output to a tab separated file")
    parser.add_argument('fileName', metavar="FILENAME", type=str, nargs=1, help='the log files to convert')
    parser.add_argument('--debug', dest='debugMode', action='store_true',  help='Enable debuging mode')
    parser.add_argument('-o', '--output', dest='output', type=str, nargs=1, required=True, help='output file name')
    args = parser.parse_args()
   
    
    fileName = args.fileName[0]
    outputFileName = args.output[0]
    if (args.debugMode):
        print ("DEBUG MODE ENABLE")
        global debugMode
        debugMode = True

    result = ""

    printLog("Input file name = " + str(args.fileName))
    printLog("Output file name = " + str(args.output))

    # Reading the input file
    if (os.path.exists(fileName)):
        printLog("File " + fileName + "Exists")
        with open(fileName, "r") as f:
            try:
                linesCounter = 0
                for line in f:
                    linesCounter = linesCounter + 1
                    if ( re.search("OMPVV_INFO", line) ):
                        result = result + line
                printLog("Len of File" + fileName + " = " + str(linesCounter))
            except: 
                print("ERROR Opening the file " + fileName)

    result = cleanUp(result)
    
    # writing the output file
    with open(outputFileName, "w") as f: 
        try:
            printLog("Writing output file " + outputFileName + " containing " + str(result.count("\n")) + " lines")
            f.write(result)
        except Exception as e :
            print ("ERROR writing the file " + outputFileName + " " + str(e))

if __name__ == "__main__":
    main()
